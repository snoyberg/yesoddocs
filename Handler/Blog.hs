{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.Blog
    ( getBlogR
    , getBlogPostR
    ) where

import Wiki
import Handler.ShowMap (loadTree, showTree)
import Util

getBlogR :: UserHandle -> Handler ()
getBlogR handle = do
    (uid, _) <- runDB $ getBy404 $ UniqueHandle handle
    posts <- runDB $ selectList [BlogOwnerEq uid] [BlogPostedDesc] 1 0
    post <-
        case posts of
            [] -> notFound
            (_, post):_ -> return post
    redirect RedirectTemporary $ BlogPostR handle $ blogSlug post

getBlogPostR :: UserHandle -> BlogSlug -> Handler RepHtml
getBlogPostR handle slug = do
    blog <- getBlogPost handle slug
    user <- runDB $ get404 $ blogOwner blog
    let tmid = blogMap blog
    tree <- loadTree tmid
    let showMap = $(widgetFile "show-map")
    defaultLayout $(widgetFile "blog")
