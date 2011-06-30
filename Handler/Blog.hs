{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.Blog
    ( getBlogR
    , getBlogPostR
    , getBlogPostNoDateR
    , getBlogFeedR
    ) where

import Wiki
import Handler.ShowMap (loadTree, showTree)
import Util
import Yesod.Feed
import Data.List (groupBy)
import Data.Function (on)
import Data.Text (pack)
import Control.Arrow ((&&&))

getBlogR :: Handler ()
getBlogR = do
    -- FIXME admin interface when appropriate
    posts <- runDB $ selectList [] [Desc BlogPosted, LimitTo 1]
    post <-
        case posts of
            [] -> notFound
            (_, post):_ -> return post
    redirect RedirectTemporary $ BlogPostR (blogYear post) (blogMonth post) $ blogSlug post

type Archive = [((Int, Int), [AEntry])]
data AEntry = AEntry
    { aeTitle :: Text
    , aeLink :: WikiRoute
    , aeDate :: Text
    }

getBlogPostR :: Int -> Month -> BlogSlugT -> Handler RepHtml
getBlogPostR year month slug = do
    let curr = BlogPostR year month slug
    blog <- getBlogPost year month slug
    archive' <- runDB $ selectList [] [Desc BlogPosted] >>= mapM (\(_, b) -> do
        tmap <- get404 $ blogMap b
        let y = blogYear b
            m = blogMonth b
            Month m' = m
        return ((y, m'), AEntry (tMapTitle tmap) (BlogPostR y m $ blogSlug b) (pack $ prettyDate $ blogPosted b)))
    let archive :: Archive
        archive = map (fst . head &&& map snd) $ groupBy ((==) `on` fst) archive'
    tmap <- runDB $ get404 $ blogMap blog
    user <- runDB $ get404 $ tMapOwner tmap
    let tmid = blogMap blog
    tree <- loadTree tmid
    let showMap = $(widgetFile "show-map")
    defaultLayout $ do
        addScript $ StaticR jquery_js
        addScript $ StaticR jquery_cookie_js
        addScript $ StaticR jquery_treeview_js
        $(widgetFile "blog")

getBlogPostNoDateR :: BlogSlugT -> Handler ()
getBlogPostNoDateR slug = do
    x <- runDB $ selectList [BlogSlug ==. slug] [Desc BlogPosted, LimitTo 1]
    case x of
        (_, y):_ -> redirect RedirectTemporary $ BlogPostR (blogYear y) (blogMonth y) slug
        [] -> notFound

getBlogFeedR :: Handler RepAtomRss
getBlogFeedR = do
    x <- runDB $ selectList [] [Desc BlogPosted, LimitTo 10]
    updated <-
        case x of
            [] -> liftIO getCurrentTime
            (_, y):_ -> return $ blogPosted y
    render <- getUrlRenderParams
    let go (_, e) = do
            blog <- getBlogPost (blogYear e) (blogMonth e) (blogSlug e)
            tmap <- runDB $ get404 $ blogMap blog
            let title = tMapTitle tmap
            tree <- loadTree $ blogMap blog
            return FeedEntry
                { feedEntryLink = BlogPostR (blogYear e) (blogMonth e) (blogSlug e)
                , feedEntryUpdated = blogPosted e
                , feedEntryTitle = title
                , feedEntryContent = showTree 2 tree render
                }
    entries <- mapM go x
    newsFeed Feed
        { feedTitle = "Yesod Web Framework"
        , feedLinkSelf = BlogFeedR
        , feedLinkHome = RootR
        , feedDescription = "Yesod Web Framework"
        , feedLanguage = "en"
        , feedUpdated = updated
        , feedEntries = entries
        }
