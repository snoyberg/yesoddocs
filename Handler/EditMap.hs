{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.EditMap
    ( getMapListR
    , getEditMapR
    , postEditMapR
    ) where

import Wiki
import Control.Monad (unless)
import Data.Aeson (json, Value (..))
import Data.Text.Encoding (encodeUtf8)
import Data.Attoparsec (parse, maybeResult)
import qualified Data.Map as Map
import qualified Data.Vector as V
import qualified Data.Text as T

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

getMapListR :: Handler RepHtml
getMapListR = do
    aid <- requireAuthId
    tms <- runDB $ selectList [TMapOwnerEq aid] [] 0 0
    defaultLayout $(widgetFile "map-list")

getEditMapR :: TMapId -> Handler RepHtml
getEditMapR mid = do
    (aid, user) <- requireAuth
    tm <- runDB $ get404 mid
    unless (aid == tMapOwner tm || userAdmin user) $ permissionDeniedI MsgNotYourMap
    topics <- runDB $ selectList [TopicOwnerEq aid] [] 0 0
    tree <- loadTM mid
    let treeTopics = getTopics tree
    -- FIXME list maps also
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
    text <- runInputPost $ ireq textField "tree"
    case maybeResult (parse json $ encodeUtf8 text) >>= go of
        Nothing -> invalidArgsI [MsgInvalidJsonInput]
        Just x -> do
            runDB $ do
                deleteWhere [TMapNodeMapEq tmid]
                mapM_ (add Nothing) $ zip [1..] x
            setMessageI MsgMapUpdated
            redirect RedirectTemporary $ EditMapR tmid
  where
    add parent (pos, SM tid children) = do
        let tmn = TMapNode tmid parent pos (Just tid) Nothing Nothing
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
