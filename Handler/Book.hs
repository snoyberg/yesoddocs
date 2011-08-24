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

showTOC :: [TOC] -> Widget
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

loadTOC :: Int -> ([MapNodeSlug] -> WikiRoute) -> TMapId -> Handler [TOC]
loadTOC depth0 toRoute =
    runDB . go' id depth0
  where
    go' front depth tmid = selectList [TMapNodeMap ==. tmid, TMapNodeParent ==. Nothing] [Asc TMapNodePosition] >>= (fmap concat . mapM (go front depth))
    go front depth (_, TMapNode
        { tMapNodeCmap = Just submap
        , tMapNodeSlug = slug
        }) = go' (front . (:) slug) depth submap
    go front depth (mnid, mn) = do
        let link = Just $ toRoute $ front [tMapNodeSlug mn]
        title <-
            case tMapNodeCtopic mn of
                Just tid -> topicTitle <$> get404 tid
                Nothing -> return "" -- FIXME
        children <-
            if depth <= 1
                then return []
                else selectList [TMapNodeParent ==. Just mnid] [Asc TMapNodePosition] >>= mapM (go front $ depth - 1)
        return [TOC link title $ concat children]

getBookR :: Handler RepHtml
getBookR = do
    book <- runDB getBook
    let toRoute [] = error "in getBookR: toRoute received an empty list"
        toRoute (x:xs) = BookChapterR x xs
    tocs <- loadTOC (bookChunking book) toRoute (bookMap book)
    tm <- runDB $ get404 $ bookMap book
    mtopic <-
        case bookTopic book of
            Nothing -> return []
            Just tid -> do
                x <- fmap (map snd) $ runDB $ selectList [TopicContentTopic ==. tid] [Desc TopicContentChanged, LimitTo 1]
                return $ map (\y -> (tid, y)) x
    defaultLayout $(widgetFile "book")

getBookChapterR :: MapNodeSlug -> MapNodeSlugs -> Handler RepHtml
getBookChapterR mnslug mnslugs = do
    -- FIXME show TOC for shallow chapters
    (mn, tree) <- runDB $ do
        (mnid, mn) <- getMapNode mnslug mnslugs
        tree <- loadTreeNode (mnid, mn)
        return (mn, tree)
    defaultLayout $ do
        addLucius $(luciusFile "book")
        addLucius $(luciusFile "show-map")
        addHamlet $ showTree 2 tree
        comments
