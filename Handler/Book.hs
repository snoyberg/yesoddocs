{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.Book
    ( getBookR
    , getBookChapterR
    ) where

import Wiki
import Util (renderContent)
import Handler.ShowMap (loadTreeNode, showTree)
import Handler.Topic (comments)

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

loadTOC :: Int -> (MapNodeSlug -> WikiRoute) -> TMapId -> Handler [TOC]
loadTOC depth0 toRoute tmid =
    runDB $ selectList [TMapNodeMapEq tmid, TMapNodeParentEq Nothing] [TMapNodePositionAsc] 0 0 >>= mapM (go depth0)
  where
    go depth (mnid, mn) = do
        let link = Just $ toRoute $ tMapNodeSlug mn
        title <-
            case tMapNodeCtopic mn of
                Just tid -> topicTitle <$> get404 tid
                Nothing -> return "" -- FIXME
        children <-
            if depth <= 1
                then return []
                else selectList [TMapNodeParentEq $ Just mnid] [TMapNodePositionAsc] 0 0 >>= mapM (go $ depth - 1)
        return $ TOC link title children

getBookR :: Handler RepHtml
getBookR = do
    book <- getBook
    tocs <- loadTOC (bookChunking book) BookChapterR (bookMap book)
    tm <- runDB $ get404 $ bookMap book
    mtopic <-
        case bookTopic book of
            Nothing -> return []
            Just tid -> do
                x <- fmap (map snd) $ runDB $ selectList [TopicContentTopicEq tid] [TopicContentChangedDesc] 1 0
                return $ map (\y -> (tid, y)) x
    defaultLayout $(hamletFile "book")

getBookChapterR :: MapNodeSlug -> Handler RepHtml
getBookChapterR mnslug = do
    -- FIXME show TOC for shallow chapters
    book <- getBook
    (mn, tree) <- runDB $ do
        (mnid, mn) <- getBy404 $ UniqueMapNode (bookMap book) mnslug
        tree <- loadTreeNode (mnid, mn)
        return (mn, tree)
    defaultLayout $ do
        addLucius $(luciusFile "book")
        addLucius $(luciusFile "show-map")
        addHamlet $ showTree 2 [tree]
        comments
