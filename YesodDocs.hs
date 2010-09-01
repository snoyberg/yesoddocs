{-# LANGUAGE TypeFamilies, QuasiQuotes, TemplateHaskell, OverloadedStrings #-}

module YesodDocs where

import Yesod
import Yesod.Helpers.Static
import Yesod.Form.Jquery
import Settings
import Text.Pandoc
import Language.Haskell.HsColour hiding (string)
import Language.Haskell.HsColour.Colourise (defaultColourPrefs)
import Entry
import Control.Arrow ((&&&))
import Data.List (groupBy)
import Data.Function (on)
import qualified System.IO.UTF8 as U

data YesodDocs = YesodDocs
    { getStatic :: Static
    , getEntries :: [Entry]
    }

mkYesod "YesodDocs" [$parseRoutes|
/static StaticR Static getStatic

/ HomeR GET
/five-minutes FiveMinutesR GET
/articles ArticlesR GET

/book BookR GET
/book/#String ChapterR GET

/examples ExamplesR GET
/examples/#String ExampleR GET

/screencasts ScreencastsR GET

/synopsis/web-routes-quasi SynWrqR GET
/synopsis/persistent SynPerR GET
/synopsis/hamlet SynHamR GET

/blog BlogR GET
/blog/#String EntryR GET
|]

navLinks =
    [ ("Home" :: String, Right HomeR)
    , ("Blog", Right BlogR)
    , ("Yesod in 5 Minutes", Right FiveMinutesR)
    , ("Book", Right BookR)
    , ("Screencasts", Right ScreencastsR)
    , ("Examples", Right ExamplesR)
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
instance YesodJquery YesodDocs where
    urlJqueryJs _ = Left $ StaticR jquery_js
    urlJqueryUiJs _ = Left $ StaticR jquery_ui_js
    urlJqueryUiCss  _ = Left $ StaticR jquery_ui_css

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
    raw <- liftIO $ U.readFile $ "book/" ++ chapter ++ ".markdown"
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

examples :: [(String, String)]
examples =
    [ ("blog", "Blog")
    , ("ajax", "Ajax")
    , ("form", "Form")
    , ("widgets", "Widgets")
    , ("pretty-yaml", "Pretty YAML")
    , ("i18n", "Internationalization")
    ]

getExamplesR = do
    y <- getYesod
    defaultLayout $ do
        setTitle "Yesod Code Examples"
        addScriptEither $ urlJqueryJs y
        addScriptEither $ urlJqueryUiJs y
        addStylesheetEither $ urlJqueryUiCss y
        addJavascript $(juliusFile "examples")
        addStyle $(cassiusFile "examples")
        addBody $(hamletFile "examples")
        addStylesheet $ StaticR hscolour_css

getExampleR name = do
    title <- maybe notFound return $ lookup name examples
    raw <- liftIO $ U.readFile $ "yesod/tutorial/" ++ name ++ ".lhs"
    let html = hscolour CSS defaultColourPrefs True False title True raw
    return $ RepHtml $ toContent html

getScreencastsR = do
    raw <- liftIO $ U.readFile "screencasts.html"
    y <- getYesod
    defaultLayout $ do
        setTitle "Yesod Web Framework Screencasts"
        addScriptEither $ urlJqueryJs y
        addScriptEither $ urlJqueryUiJs y
        addStylesheetEither $ urlJqueryUiCss y
        addJavascript $(juliusFile "screencasts")
        addStyle $(cassiusFile "screencasts")
        addBody $ const $ preEscapedString raw

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

getSynWrqR = do
    let title = "web-routes-quasi Synopsis"
    raw <- liftIO $ U.readFile "synopsis/web-routes-quasi.markdown"
    let pandoc = readMarkdown defaultParserState raw
    let content = preEscapedString $ writeHtmlString defaultWriterOptions pandoc
    defaultLayout $ do
        setTitle title
        addBody $(hamletFile "synopsis")

synLhs file title' = do
    let title = title' ++ " Synopsis"
    raw <- liftIO $ U.readFile $ "synopsis/" ++ file ++ ".lhs"
    let content = preEscapedString
                $ hscolour CSS defaultColourPrefs True False title True raw
    defaultLayout $ do
        setTitle $ string title
        addStylesheet $ StaticR hscolour_css
        addBody $(hamletFile "synopsis")

getSynPerR = synLhs "persistent" "Persistent"
getSynHamR = synLhs "hamlet" "Hamlet"

getBlogR = do
    y <- getYesod
    redirect RedirectTemporary $ EntryR $ entrySlug $ head $ getEntries y
    return ()

getEntryR slug = do
    let isCurrent x = x == slug
    y <- getYesod
    entry <-
        case filter (\x -> entrySlug x == slug) $ getEntries y of
            [] -> notFound
            e:_ -> return e
    let navbar = mkNavbar $ getEntries y
    defaultLayout $ do
        setTitle $ string $ "Yesod Blog: " ++ entryTitle entry
        addBody $(hamletFile "blog")
        addStyle $(cassiusFile "blog")
        addScriptEither $ urlJqueryJs y
        addScript $ StaticR jquery_cookie_js
        addScript $ StaticR jquery_treeview_js
        addJavascript $(juliusFile "blog")
  where
    mkNavbar :: [Entry] -> [(String, [Entry])]
    mkNavbar = map (entryYearMonth . head &&& id) . groupBy ((==) `on` entryYearMonth)

withYesodDocs f = do
    entries <- loadEntries
    let static = fileLookupDir "static" typeByExt
    app <- toWaiApp $ YesodDocs static entries
    f app
