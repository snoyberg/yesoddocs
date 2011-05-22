{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.ShowMap
    ( getShowMapR
    , getShowMapTopicR
    , loadTree
    , showTree
    , loadTreeNode
    ) where

import Wiki
import Handler.Topic (getTopicR)
import Util (renderContent)
import Database.Persist.GenericSql (SqlPersist)

data Tree = Tree
    { treeTopicId :: Maybe TopicId -- FIXME TMapNodeId
    , treeTitle :: Text
    , treeContent :: Maybe TopicContent
    , treeChildren :: [Tree]
    }

showTree :: Int -> TMapId -> [Tree] -> Widget ()
showTree depth tmid trees = [whamlet|
$forall tree <- trees
    <section>
        \<h#{show depth}>
        $maybe tid <- treeTopicId tree
            <a .topic-link href=@{ShowMapTopicR tmid tid}>#{treeTitle tree}
        $nothing
            \#{treeTitle tree}
        \</h#{show depth}>
        $maybe c <- treeContent tree
            \#{renderContent (topicContentFormat c) (topicContentContent c)}
        ^{showTree (incr depth) tmid $ treeChildren tree}
|]
  where
    incr 6 = 6
    incr i = i + 1

loadTree :: TMapId -> Handler [Tree]
loadTree tmid =
    runDB $ selectList [TMapNodeMapEq tmid, TMapNodeParentEq Nothing] [TMapNodePositionAsc] 0 0 >>= mapM loadTreeNode

loadTreeNode :: (TMapNodeId, TMapNode) -> SqlPersist (GGHandler sub Wiki IO) Tree
loadTreeNode (tmnid, tmn) = do
    c <- selectList [TMapNodeParentEq $ Just tmnid] [] 0 0 >>= mapM loadTreeNode
    title <-
        case tMapNodeCtopic tmn of
            Nothing -> return ""
            Just tid -> do
                t <- get404 tid
                return $ topicTitle t
    content <-
        case tMapNodeCtopic tmn of
            Nothing -> return Nothing
            Just tid -> do
                x <- selectList [TopicContentTopicEq tid] [TopicContentChangedDesc] 1 0
                case x of
                    (_, y):_ -> return $ Just y
                    [] -> return Nothing
    return Tree
        { treeTopicId = tMapNodeCtopic tmn
        , treeTitle = title
        , treeChildren = c
        , treeContent = content
        }

getShowMapR :: TMapId -> Handler RepHtml
getShowMapR tmid = do
    tree <- loadTree tmid
    defaultLayout $(widgetFile "show-map")

getShowMapTopicR :: TMapId -> TopicId -> Handler RepHtml
getShowMapTopicR _tmid topicid = getTopicR topicid
