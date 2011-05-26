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
import Yesod.Helpers.Feed

getBlogR :: Handler ()
getBlogR = do
    -- FIXME admin interface when appropriate
    posts <- runDB $ selectList [] [BlogPostedDesc] 1 0
    post <-
        case posts of
            [] -> notFound
            (_, post):_ -> return post
    redirect RedirectTemporary $ BlogPostR (blogYear post) (blogMonth post) $ blogSlug post

getBlogPostR :: Int -> Int -> BlogSlug -> Handler RepHtml
getBlogPostR year month slug = do
    blog <- getBlogPost year month slug
    tmap <- runDB $ get404 $ blogMap blog
    user <- runDB $ get404 $ tMapOwner tmap
    let tmid = blogMap blog
    tree <- loadTree tmid
    let showMap = $(widgetFile "show-map")
    defaultLayout $(widgetFile "blog")

getBlogPostNoDateR :: BlogSlug -> Handler ()
getBlogPostNoDateR slug = do
    x <- runDB $ selectList [BlogSlugEq slug] [BlogPostedDesc] 1 0
    case x of
        (_, y):_ -> redirect RedirectTemporary $ BlogPostR (blogYear y) (blogMonth y) slug
        [] -> notFound

getBlogFeedR :: Handler RepAtomRss
getBlogFeedR = do
    x <- runDB $ selectList [] [BlogPostedDesc] 10 0
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
                , feedEntryContent = showTree 2 (blogMap blog) tree render
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
