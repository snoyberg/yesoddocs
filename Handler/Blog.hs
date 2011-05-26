{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.Blog
    ( getBlogR
    , getBlogPostR
    , getBlogPostNoDateR
    ) where

import Wiki
import Handler.ShowMap (loadTree, showTree)
import Util

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
