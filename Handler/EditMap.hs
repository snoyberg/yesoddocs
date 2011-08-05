{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.EditMap
    ( getEditMapR
    , postEditMapR
    , postMapLabelsR
    , postEditMapNameR
    , postMakeSubMapR
    ) where

import Wiki
import Control.Monad (unless, when)
import Data.Aeson (json, Value (..))
import Data.Text.Encoding (encodeUtf8)
import Data.Attoparsec (parse, maybeResult)
import qualified Data.Map as Map
import qualified Data.Vector as V
import qualified Data.Text as T
import Handler.Labels (getLTree)
import Handler.Topic (showLTree)
import Data.Maybe (mapMaybe)
import System.Random (randomRIO)
import Data.Text (pack)
import Handler.Settings (filterWidget)

data TM = TM
    { tmTitle :: Maybe Text
    , tmTopic :: Maybe TopicId
    , tmMap :: Maybe TMapId
    , tmChildren :: [TM]
    , tmSlug :: MapNodeSlug
    }

showTMs :: [TM] -> Widget ()
showTMs tms = [whamlet|
<ul>
    $forall tm <- tms
        $maybe t <- tmTopic tm
            <li .node #_topic#{toSinglePiece t} slug=#{unMapNodeSlug $ tmSlug tm} .realtopic>
                ^{showTMs $ tmChildren tm}
        $maybe t <- tmMap tm
            <li .node #_map#{toSinglePiece t} slug=#{unMapNodeSlug $ tmSlug tm}>
                ^{showTMs $ tmChildren tm}
|]

loadTM :: TMapId -> Handler [TM]
loadTM tmid =
    runDB $ selectList [TMapNodeMap ==. tmid, TMapNodeParent ==. Nothing] [Asc TMapNodePosition] >>= mapM go
  where
    go (tmnid, tmn) = do
        children <- selectList [TMapNodeParent ==. Just tmnid] [Asc TMapNodePosition] >>= mapM go
        return TM
            { tmTitle = tMapNodeTitle tmn
            , tmTopic = tMapNodeCtopic tmn
            , tmMap = tMapNodeCmap tmn
            , tmChildren = children
            , tmSlug = tMapNodeSlug tmn
            }

getEditMapR :: TMapId -> Handler RepHtml
getEditMapR mid = do
    (aid, user) <- requireAuth
    tm <- runDB $ get404 mid
    unless (aid == tMapOwner tm || userAdmin user) $ permissionDeniedI MsgNotYourMap
    topics <- runDB $ selectList [TopicOwner ==. aid] []
    maps <- fmap (filter ((/=) mid . fst)) $ runDB $ selectList [TMapOwner ==. aid] []
    tree <- loadTM mid
    let treeTopics = getTopics tree
    ltree <- getLTree
    slabels <- runDB $ fmap (map $ mapLabelLabel . snd) $ selectList [MapLabelMap ==. mid] []
    let activeLabel = flip elem slabels
    defaultLayout $ do
        addScript $ StaticR jquery_js
        filterWidget
        $(widgetFile "edit-map")

getTopics :: [TM] -> [TopicId]
getTopics =
    concatMap go
  where
    go x = maybe id (:) (tmTopic x) $ getTopics $ tmChildren x

data SM = SM
    { smtopic :: NodeId
    , smchildren :: [SM]
    , smslug :: Maybe MapNodeSlug
    }
    deriving Show

postEditMapR :: TMapId -> Handler ()
postEditMapR tmid = do
    (aid, user) <- requireAuth
    tm <- runDB $ get404 tmid
    unless (aid == tMapOwner tm || userAdmin user) $ permissionDeniedI MsgNotYourMap
    text <- runInputPost $ ireq textField "tree"
    case maybeResult (parse json $ encodeUtf8 text) >>= go of
        Nothing -> invalidArgsI [MsgInvalidJsonInput text]
        Just x -> do
            runDB $ do
                deleteWhere [TMapNodeMap ==. tmid]
                mapM_ (add Nothing) $ zip [1..] x
            setMessageI MsgMapUpdated
            redirect RedirectTemporary $ EditMapR tmid
  where
    randomSlug = do
        str <- liftIO $ sequence $ replicate 15 $ randomRIO ('A', 'Z')
        return $ MapNodeSlug $ pack str
    add parent (pos, SM tid children mslug) = do
        slug <- maybe randomSlug return mslug
        let (topicid, mapid) =
                case tid of
                    NITopic x -> (Just x, Nothing)
                    NIMap x -> (Nothing, Just x)
        let tmn = TMapNode tmid parent pos topicid mapid Nothing slug
        tmnid <- insert tmn
        mapM_ (add $ Just tmnid) $ zip [1..] children
    go (Array x) = mapM go' $ V.toList x
    go _ = Nothing
    go' (Object o) = do
        t <- Map.lookup "topic" o
        t' <- go'' t
        c <- Map.lookup "children" o
        c' <- go c
        let slug =
                case Map.lookup "slug" o of
                    Just (String slug') -> Just $ MapNodeSlug slug'
                    _ -> Nothing
        return $ SM t' c' slug
    go' _ = Nothing
    go'' (String t)
        | "topic" `T.isPrefixOf` t = fmap NITopic $ fromSinglePiece $ T.drop 5 t
        | "map" `T.isPrefixOf` t = fmap NIMap $ fromSinglePiece $ T.drop 3 t
    go'' _ = Nothing

data NodeId = NITopic TopicId | NIMap TMapId
    deriving Show

postMapLabelsR :: TMapId -> Handler ()
postMapLabelsR mid = do
    (aid, user) <- requireAuth
    tm <- runDB $ get404 mid
    unless (aid == tMapOwner tm || userAdmin user) $ permissionDeniedI MsgNotYourMap
    (pp, _) <- runRequestBody
    let sel = mapMaybe (fromSinglePiece . fst) pp
    runDB $ do
        deleteWhere [MapLabelMap ==. mid]
        labels <- selectList [] []
        flip mapM_ labels $ \(lid', _) -> do -- FIXME use a Set?
            when (lid' `elem` sel) $
                insert (MapLabel mid lid') >> return ()
    redirect RedirectTemporary $ EditMapR mid

postEditMapNameR :: TMapId -> Handler ()
postEditMapNameR tmid = do
    (aid, user) <- requireAuth
    tm <- runDB $ get404 tmid
    unless (aid == tMapOwner tm || userAdmin user) $ permissionDeniedI MsgNotYourMap
    name <- runInputPost $ ireq textField "name"
    runDB $ update tmid [TMapTitle =. name]
    setMessageI (MsgMapTitleUpdated name)
    redirect RedirectTemporary $ EditMapR tmid

postMakeSubMapR :: TMapId -> MapNodeSlug -> Handler ()
postMakeSubMapR tmid mns = do
    (aid, user) <- requireAuth
    tm <- runDB $ get404 tmid
    unless (aid == tMapOwner tm || userAdmin user) $ permissionDeniedI MsgNotYourMap
    (mnid, mn) <- runDB $ getBy404 $ UniqueMapNode tmid mns
    case tMapNodeCtopic mn of
        Nothing -> setMessageI MsgMakeSubmapNotATopic
        Just tid -> runDB $ do
            t <- get404 tid
            now <- liftIO getCurrentTime
            tmid' <- insert $ TMap aid (topicTitle t) now
            update mnid [TMapNodeMap =. tmid', TMapNodeParent =. Nothing, TMapNodePosition =. 1]
            fixMap mnid tmid'
            _ <- insert mn
                { tMapNodeCtopic = Nothing
                , tMapNodeCmap = Just tmid'
                }
            lift $ setMessageI MsgSubmapCreated
    redirect RedirectTemporary $ EditMapR tmid
  where
    fixMap mnid tmid' = selectList [TMapNodeParent ==. Just mnid] [] >>= (mapM_ $ \(mnid', _) -> do
        update mnid' [TMapNodeMap =. tmid']
        fixMap mnid' tmid')
