{-# LANGUAGE TypeFamilies, QuasiQuotes, TemplateHaskell, OverloadedStrings #-}

import Yesod
import Yesod.Helpers.Static
import Settings
import Text.Pandoc

data YesodDocs = YesodDocs
    { getStatic :: Static
    }

mkYesod "YesodDocs" [$parseRoutes|
/static StaticR Static getStatic

/ HomeR GET
/five-minutes FiveMinutesR GET
/articles ArticlesR GET

/book BookR GET
/book/#String ChapterR GET
|]

navLinks =
    [ ("Home" :: String, Right HomeR)
    , ("Blog", Left "http://www.snoyman.com/blog/")
    , ("Yesod in 5 Minutes", Right FiveMinutesR)
    , ("Book", Right BookR)
    , ("Screencasts", Left "FIXME")
    , ("Examples", Left "FIXME")
    , ("Articles", Right ArticlesR)
    ]

instance Yesod YesodDocs where
    approot _ = ""
    defaultLayout widget = do
        curr <- getCurrentRoute
        tm <- getRouteToMaster
        let isCurrent x = fmap (Right . tm) curr == Just x
        render' <- getUrlRender
        let render (Left s) = s
            render (Right u) = render' u
        pc <- widgetToPageContent $ do
            widget
            addStyle $(cassiusFile "default-layout")
        hamletToRepHtml $(hamletFile "default-layout")

getHomeR = defaultLayout $ do
            setTitle "Yesod Web Framework for Haskell"
            addBody $(hamletFile "root")
            addStyle $(cassiusFile "root")

getFiveMinutesR = defaultLayout $ do
    setTitle "Yesod in Five Minutes"
    addBody $(hamletFile "five-minutes")

chapters =
    [ ("introduction" :: String, "Introduction" :: String)
    , ("basics", "Basics")
    , ("templates", "Templates")
    , ("handler", "The Handler Monad")
    , ("wai", "Web Application Interface")
    , ("hamlet", "Hamlet")
    , ("forms", "Forms")
    , ("deploying", "Deploying your Webapp")
    , ("persistent", "Persistent")
    , ("web-routes-quasi", "web-routes-quasi")
    , ("widgets", "Widgets")
    ]

getBookR = defaultLayout $ do
    setTitle "Yesod Web Framework Book"
    addBody $(hamletFile "book")

getChapterR chapter = do
    title <- maybe notFound return $ lookup chapter chapters
    raw <- liftIO $ readFile $ "book/" ++ chapter ++ ".markdown"
    let pandoc = readMarkdown defaultParserState raw
    let html = preEscapedString $ writeHtmlString defaultWriterOptions pandoc
    let previous = getPrev chapters
    let next = getNext chapters
    defaultLayout $ do
        setTitle $ string $ "Yesod Book: " ++ title
        addBody $(hamletFile "chapter")
  where
    getPrev (x:(y, y'):rest)
        | y == chapter = Just x
        | otherwise = getPrev $ (y, y') : rest
    getPrev _ = Nothing
    getNext ((x, _):y:rest)
        | x == chapter = Just y
        | otherwise = getNext $ y : rest
    getNext _ = Nothing

data Article = Article
    { articleUrl :: String
    , articleTitle :: String
    , articleDate :: String
    , articleDescription :: String
    }

getArticlesR =
    defaultLayout $ do
        setTitle "Yesod Articles"
        addBody $(hamletFile "articles")
  where
    articles =
        [ Article
            "http://braincrater.wordpress.com/2010/07/22/ajax-chat-app-using-yesod/"
            "Ajax Push"
            "July 22, 2010"
            "An extension of the basic chat example to use Ajax push."
        , Article
            "http://www.snoyman.com/blog/entry/handler-monad/"
            "The Handler Monad"
            "June 15, 2010"
            "Slightly out-of-date, as we've migrated back to a form of either monad transformer."
        , Article
            "http://www.snoyman.com/blog/entry/sessions/"
            "Sessions, messages and ultimate destination"
            "July 16, 2010"
            "Covers sessions and two of the built-in uses of them."
        , Article
            "http://www.snoyman.com/blog/entry/fringe-benefits-typesafe-urls/"
            "Fringe benefits of type-safe URLs"
            "June 24, 2010"
            "Covers breadcrumbs, dedicated static file servers and authorization."
        , Article
            "http://www.snoyman.com/blog/entry/restful-content/"
            "RESTful Content"
            "June 18, 2010"
            ""
        ]

main = basicHandler 3000 $ YesodDocs $ fileLookupDir "static" typeByExt
