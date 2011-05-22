{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.Book
    ( getBookR
    , getBookChapterR
    ) where

import Wiki
import Util (renderContent)
import Handler.ShowMap (loadTreeNode, showTree)

data TOC = TOC
    { tocLink :: Maybe WikiRoute
    , tocTitle :: Text
    , tocChildren :: [TOC]
    }

showTOC :: [TOC] -> Widget ()
showTOC tocs = [whamlet|
$if not $ null tocs
    <ol>
        $forall toc <- tocs
            <li>
                $maybe link <- tocLink toc
                    <a href=@{link}>#{tocTitle toc}
                $nothing
                    \#{tocTitle toc}
                ^{showTOC $ tocChildren toc}
|]

loadTOC :: Int -> (TMapNodeId -> WikiRoute) -> TMapId -> Handler [TOC]
loadTOC depth0 toRoute tmid =
    runDB $ selectList [TMapNodeMapEq tmid, TMapNodeParentEq Nothing] [TMapNodePositionAsc] 0 0 >>= mapM (go depth0)
  where
    go depth (mnid, mn) = do
        let link = Just $ toRoute mnid
        title <-
            case tMapNodeCtopic mn of
                Just tid -> topicTitle <$> get404 tid
                Nothing -> return "" -- FIXME
        children <-
            if depth <= 1
                then return []
                else selectList [TMapNodeParentEq $ Just mnid] [TMapNodePositionAsc] 0 0 >>= mapM (go $ depth - 1)
        return $ TOC link title children

getBookR :: UserHandle -> BookSlug -> Handler RepHtml
getBookR uh bs = do
    book <- getBook uh bs
    tocs <- loadTOC (bookChunking book) (BookChapterR uh bs) (bookMap book)
    tm <- runDB $ get404 $ bookMap book
    mtopic <-
        case bookTopic book of
            Nothing -> return []
            Just tid -> fmap (map snd) $ runDB $ selectList [TopicContentTopicEq tid] [TopicContentChangedDesc] 1 0
    defaultLayout $(hamletFile "book")

getBookChapterR :: UserHandle -> BookSlug -> TMapNodeId -> Handler RepHtml
getBookChapterR _ _ mnid = do
    (mn, tree) <- runDB $ do
        mn <- get404 mnid
        tree <- loadTreeNode (mnid, mn)
        return (mn, tree)
    defaultLayout $ do
        addLucius $(luciusFile "show-map")
        showTree 2 (tMapNodeMap mn) [tree]
