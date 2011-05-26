{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.EditMap
    ( getEditMapR
    , postEditMapR
    , postMapLabelsR
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

data TM = TM
    { tmTitle :: Maybe Text
    , tmTopic :: Maybe TopicId
    , tmMap :: Maybe TMapId
    , tmChildren :: [TM]
    }

showTMs :: [TM] -> Widget ()
showTMs tms = [whamlet|
<ul>
    $forall tm <- tms
        $maybe t <- tmTopic tm
            <li .node #_topic#{toSinglePiece t}>
                ^{showTMs $ tmChildren tm}
|]

loadTM :: TMapId -> Handler [TM]
loadTM tmid =
    runDB $ selectList [TMapNodeMapEq tmid, TMapNodeParentEq Nothing] [TMapNodePositionAsc] 0 0 >>= mapM go
  where
    go (tmnid, tmn) = do
        children <- selectList [TMapNodeParentEq $ Just tmnid] [TMapNodePositionAsc] 0 0 >>= mapM go
        return TM
            { tmTitle = tMapNodeTitle tmn
            , tmTopic = tMapNodeCtopic tmn
            , tmMap = tMapNodeCmap tmn
            , tmChildren = children
            }

getEditMapR :: TMapId -> Handler RepHtml
getEditMapR mid = do
    (aid, user) <- requireAuth
    tm <- runDB $ get404 mid
    unless (aid == tMapOwner tm || userAdmin user) $ permissionDeniedI MsgNotYourMap
    topics <- runDB $ selectList [TopicOwnerEq aid] [] 0 0
    tree <- loadTM mid
    let treeTopics = getTopics tree
    -- FIXME list maps also
    ltree <- getLTree
    slabels <- runDB $ fmap (map $ mapLabelLabel . snd) $ selectList [MapLabelMapEq mid] [] 0 0
    let activeLabel = flip elem slabels
    defaultLayout $ do
        addScript $ StaticR jquery_js
        $(widgetFile "edit-map")

getTopics :: [TM] -> [TopicId]
getTopics =
    concatMap go
  where
    go x = maybe id (:) (tmTopic x) $ getTopics $ tmChildren x

data SM = SM
    { smtopic :: TopicId
    , smchildren :: [SM]
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
                deleteWhere [TMapNodeMapEq tmid]
                mapM_ (add Nothing) $ zip [1..] x
            setMessageI MsgMapUpdated
            redirect RedirectTemporary $ EditMapR tmid
  where
    randomSlug = do
        str <- liftIO $ sequence $ replicate 15 $ randomRIO ('A', 'Z')
        return $ MapNodeSlug $ pack str
    add parent (pos, SM tid children) = do
        slug <- randomSlug -- FIXME keep some consistency
        let tmn = TMapNode tmid parent pos (Just tid) Nothing Nothing slug
        tmnid <- insert tmn
        mapM_ (add $ Just tmnid) $ zip [1..] children
    go (Array x) = mapM go' $ V.toList x
    go _ = Nothing
    go' (Object o) = do
        t <- Map.lookup "topic" o
        t' <- go'' t
        c <- Map.lookup "children" o
        c' <- go c
        return $ SM t' c'
    go' _ = Nothing
    go'' (String t)
        | "topic" `T.isPrefixOf` t = fromSinglePiece $ T.drop 5 t
    go'' _ = Nothing

postMapLabelsR :: TMapId -> Handler ()
postMapLabelsR mid = do
    (aid, user) <- requireAuth
    tm <- runDB $ get404 mid
    unless (aid == tMapOwner tm || userAdmin user) $ permissionDeniedI MsgNotYourMap
    (pp, _) <- runRequestBody
    let sel = mapMaybe (fromSinglePiece . fst) pp
    runDB $ do
        deleteWhere [MapLabelMapEq mid]
        labels <- selectList [] [] 0 0
        flip mapM_ labels $ \(lid', _) -> do -- FIXME use a Set?
            when (lid' `elem` sel) $
                insert (MapLabel mid lid') >> return ()
    redirect RedirectTemporary $ EditMapR mid
