{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.ShowMap
    ( getShowMapR
    , getShowMapTopicR
    ) where

import Wiki
import Handler.Topic (getTopicR)

data Tree = Tree
    { treeTopicId :: Maybe TopicId -- FIXME TMapNodeId
    , treeTitle :: Text
    , treeChildren :: [Tree]
    }

showTree :: TMapId -> [Tree] -> Widget ()
showTree tmid trees = [whamlet|
<ol>
    $forall tree <- trees
        <li>
            <span>
                $maybe tid <- treeTopicId tree
                    <a href=@{ShowMapTopicR tmid tid}>#{treeTitle tree}
                $nothing
                    #{treeTitle tree}
            $if not $ null $ treeChildren tree
                ^{showTree tmid $ treeChildren tree}
|]

loadTree :: TMapId -> Handler [Tree]
loadTree tmid =
    runDB $ selectList [TMapNodeMapEq tmid, TMapNodeParentEq Nothing] [TMapNodePositionAsc] 0 0 >>= mapM (go True)
  where
    go deeper (tmnid, tmn) = do
        c <- if deeper
                then selectList [TMapNodeParentEq $ Just tmnid] [] 0 0 >>= mapM (go False)
                else return []
        title <-
            case tMapNodeCtopic tmn of
                Nothing -> return ""
                Just tid -> do
                    t <- get404 tid
                    return $ topicTitle t
        return Tree
            { treeTopicId = tMapNodeCtopic tmn
            , treeTitle = title
            , treeChildren = c
            }

getShowMapR :: TMapId -> Handler RepHtml
getShowMapR tmid = do
    tree <- loadTree tmid
    defaultLayout $(widgetFile "show-map")

getShowMapTopicR :: TMapId -> TopicId -> Handler RepHtml
getShowMapTopicR _tmid topicid = getTopicR topicid
