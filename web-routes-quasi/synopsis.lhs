---
title: Synopsis -- web-routes-quasi
---
> {-# LANGUAGE TemplateHaskell #-}
> {-# LANGUAGE QuasiQuotes #-}
> module Blog where
> 
> import Web.Routes.Quasi (parseRoutes, createRoutes)
> import Web.Routes.Site
> import Network.Wai
> import Network.Wai.Enumerator
> import Static (StaticRoutes (..), siteStatic)
> 
> import qualified Data.ByteString.Char8 as S
> import qualified Data.ByteString.Lazy.Char8 as L
> 
> data Entry = Entry
>     { entrySlug :: String
>     , entryTitle :: String
>     , entryContent :: String
>     }
> 
> data BlogArgs = BlogArgs
>     { staticPath :: FilePath
>     , blogTitle :: String
>     , blogEntries :: [Entry]
>     }
> 
> newtype MyApp url = MyApp
>     { runMyApp :: BlogArgs -> (url -> String) -> Application
>     }
> 
> $(createRoutes "BlogRoutes" ''Application ''BlogArgs "runMyApp" [$parseRoutes|
> /                Home       GET
> /entry/$         EntryRoute GET
> /fake/#          Fake
> /static          Static     StaticRoutes siteStaticBlog
> |])
> 
> siteStaticBlog :: BlogArgs -> Site StaticRoutes Application
> siteStaticBlog = siteStatic . staticPath
> 
> handleFake :: Integer -> MyApp BlogRoutes
> handleFake = undefined
> 
> getHome :: MyApp BlogRoutes
> getHome = MyApp $ \ba f _ -> return Response
>     { status = Status302
>     , responseHeaders = [(Location, S.pack $ f $ EntryRoute $ entrySlug
>                                            $ head $ blogEntries ba)]
>     , responseBody = Right $ fromLBS $ L.pack ""
>     }
> 
> getEntryRoute :: String -> MyApp BlogRoutes
> getEntryRoute slug = MyApp $ \ba f _ ->
>     case filter (\x -> entrySlug x == slug) $ blogEntries ba of
>         [] -> return Response
>                 { status = Status404
>                 , responseHeaders = []
>                 , responseBody = Right $ fromLBS $ L.pack "Not found"
>                 }
>         (e:_) -> return Response
>                 { status = Status200
>                 , responseHeaders = [(ContentType, S.pack "text/html")]
>                 , responseBody = Right $ fromLBS $ L.pack $ concat
>                     [ "<!DOCTYPE html>\n<html><head><title>"
>                     , blogTitle ba
>                     , "</title><link rel=\"stylesheet\" href=\""
>                     , f $ Static $ StaticRoutes ["style.css"]
>                     , "\"></head><body><ul>"
>                     , concatMap (\e' -> concat
>                         [ "<li><a href=\""
>                         , f $ EntryRoute $ entrySlug e'
>                         , "\">"
>                         , entryTitle e'
>                         , "</a></li>"
>                         ]) $ blogEntries ba
>                     , "</ul><h1>"
>                     , entryTitle e
>                     , "</h1><p>"
>                     , entryContent e
>                     , "</p></body></html>"
>                     ]
>                 }
