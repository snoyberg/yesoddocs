{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.Blog
    ( getBlogPostR
    ) where

import Wiki
import Handler.ShowMap (loadTree, showTree)
import Util

getBlogPostR :: UserHandle -> BlogSlug -> Handler RepHtml
getBlogPostR handle slug = do
    blog <- getBlogPost handle slug
    user <- runDB $ get404 $ blogOwner blog
    let tmid = blogMap blog
    tree <- loadTree tmid
    let showMap = $(widgetFile "show-map")
    defaultLayout $(widgetFile "blog")
