{-# LANGUAGE TypeFamilies, QuasiQuotes, TemplateHaskell, OverloadedStrings #-}

module YesodDocs where

import Yesod
import Yesod.Helpers.Static
import Yesod.Helpers.Sitemap
import Yesod.Helpers.AtomFeed
import Yesod.Form.Jquery
import Settings
import Text.Pandoc (readMarkdown, defaultParserState, writeHtmlString, defaultWriterOptions)
import Language.Haskell.HsColour hiding (string)
import Language.Haskell.HsColour.Colourise (defaultColourPrefs)
import qualified Language.Haskell.HsColour.CSS as CSS
import Entry
import Control.Arrow ((&&&))
import Data.List (groupBy, stripPrefix)
import Data.Maybe (fromMaybe)
import Data.Function (on)
import qualified System.IO.UTF8 as U
import Data.Time
import Book
import qualified Data.Text.Lazy as T

data YesodDocs = YesodDocs
    { getStatic :: Static
    , getEntries :: [Entry]
    , getBook :: Book
    }

mkYesod "YesodDocs" [$parseRoutes|
/favicon.ico FaviconR GET
/robots.txt RobotsR GET
/sitemap.xml SitemapR GET
/feed FeedR GET

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
    , ("Wiki", Left "http://wiki.yesodweb.com/")
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
            addCassius $(cassiusFile "default-layout")
        hamletToRepHtml $(hamletFile "default-layout")
instance YesodJquery YesodDocs where
    urlJqueryJs _ = Left $ StaticR jquery_js
    urlJqueryUiJs _ = Left $ StaticR jquery_ui_js
    urlJqueryUiCss  _ = Left $ StaticR jquery_ui_css

getHomeR = defaultLayout $ do
            setTitle "Yesod Web Framework for Haskell"
            addHamlet $(hamletFile "root")
            addHtmlHead [$hamlet|
%meta!name=description!value="Yesod Web Framework for Haskell. Create RESTful web apps with type safety."
|]
            addCassius $(cassiusFile "root")

getFiveMinutesR = defaultLayout $ do
    setTitle "Yesod in Five Minutes"
    addHamlet $(hamletFile "five-minutes")

getBookR = defaultLayout $ do
    setTitle "Yesod Web Framework Book"
    book <- liftHandler $ fmap getBook getYesod
    let unpack = T.unpack
    let chapters = concatMap partChapters $ bookParts book
    addHamlet $(hamletFile "book")
    addCassius $(cassiusFile "book")

getChapterR :: String -> GHandler YesodDocs YesodDocs RepHtml
getChapterR slug = do
    book <- fmap getBook getYesod
    chapter <-
        case filter (\x -> chapterSlug x == T.pack slug)
                $ concatMap partChapters $ bookParts book of
            [] -> notFound
            x:_ -> return x
    let previous = Nothing -- FIXME getPrev chapters
    let next = Nothing -- FIXME getNext chapters
    y <- getYesod
    let title = T.unpack $ chapterTitle chapter
    let unpack = T.unpack
    let html = chapterToHtml chapter
    defaultLayout $ do
        setTitle $ string $ "Yesod Book: " ++ title
        addScriptEither $ urlJqueryJs y
        addHamlet $(hamletFile "chapter")
        addCassius $(cassiusFile "book")
        addJulius $(juliusFile "chapter")
        addCassius $(cassiusFile "chapter")
        addStylesheet $ StaticR hscolour_css
        -- addScript $ StaticR hyphenate_js
  where
  {-
    getPrev (x:yc@(Chapter y y' _):rest)
        | y == chapter = Just x
        | otherwise = getPrev $ yc : rest
    getPrev _ = Nothing
    getNext ((Chapter x _ _):y:rest)
        | x == chapter = Just y
        | otherwise = getNext $ y : rest
    getNext _ = Nothing
    -}
    go ('~':filename) = snippet filename
    go x = return x

snippet :: String -> IO String
snippet filename = do
    raw <- U.readFile $ "snippets/" ++ filename ++ ".hs"
    let raw' = unlines $ go False $ lines raw
    return $ "<pre class='code-block'><code>" ++ (unlines $ zipWith go' [1..] $ lines
           $ CSS.hscolour False raw')
  where
    go _ [] = []
    go False ("-- START":rest) = go True rest
    go True ("-- STOP":rest) = go False rest
    go False (_:rest) = go False rest
    go True (x:rest) = x : go True rest
    go' _ "</pre>" = "</code></pre>"
    go' i s = concat
        [ "<span class='line-number'>"
        , show i
        , "</span>"
        , removePre s
        ]
    removePre s = fromMaybe s $ stripPrefix "<pre>" s

examples :: [(String, String)]
examples =
    [ ("blog", "Blog")
    , ("ajax", "Ajax")
    , ("form", "Form")
    , ("widgets", "Widgets")
    , ("generalized-hamlet", "Generalized Hamlet")
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
        addJulius $(juliusFile "examples")
        addCassius $(cassiusFile "examples")
        addHamlet $(hamletFile "examples")
        addStylesheet $ StaticR hscolour_css

colorize title raw isLit = hscolour CSS defaultColourPrefs True False title isLit raw

getExampleR name = do
    title <- maybe notFound return $ lookup name examples
    raw <- liftIO $ U.readFile $ "yesod/tutorial/" ++ name ++ ".lhs"
    let html = colorize title raw True
    return $ RepHtml $ toContent html

getScreencastsR = do
    raw <- liftIO $ U.readFile "screencasts.html"
    y <- getYesod
    defaultLayout $ do
        setTitle "Yesod Web Framework Screencasts"
        addScriptEither $ urlJqueryJs y
        addScriptEither $ urlJqueryUiJs y
        addStylesheetEither $ urlJqueryUiCss y
        addJulius $(juliusFile "screencasts")
        addCassius $(cassiusFile "screencasts")
        addHamlet $ const $ preEscapedString raw

data Article = Article
    { articleUrl :: String
    , articleTitle :: String
    , articleDate :: String
    , articleDescription :: String
    }

getArticlesR =
    defaultLayout $ do
        setTitle "Yesod Articles"
        addHamlet $(hamletFile "articles")
  where
    articles =
        [ Article
            "http://docs.yesodweb.com/blog/custom-forms/"
            "Custom Forms and the Form Monad"
            "October 20, 2010"
            "Explains a new way of creating forms introduced in Yesod 0.6."
        , Article
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
        addHamlet $(hamletFile "synopsis")

synLhs file title' = do
    let title = title' ++ " Synopsis"
    raw <- liftIO $ U.readFile $ "synopsis/" ++ file ++ ".lhs"
    let content = preEscapedString $ colorize title raw True
    defaultLayout $ do
        setTitle $ string title
        addStylesheet $ StaticR hscolour_css
        addHamlet $(hamletFile "synopsis")

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
        addHamlet $(hamletFile "blog")
        addCassius $(cassiusFile "blog")
        addScriptEither $ urlJqueryJs y
        addScript $ StaticR jquery_cookie_js
        addScript $ StaticR jquery_treeview_js
        addJulius $(juliusFile "blog")
  where
    mkNavbar :: [Entry] -> [(String, [Entry])]
    mkNavbar = map (entryYearMonth . head &&& id) . groupBy ((==) `on` entryYearMonth)

getFaviconR = sendFile "image/x-icon" "favicon.ico" >> return ()

getRobotsR = robots SitemapR

getSitemapR = do
    y <- getYesod
    sitemap $ home : screencasts : examples : book
            : map go1 (concatMap partChapters $ bookParts $ getBook y)
            ++ map go2 (getEntries y)
  where
    date = UTCTime (read "2010-09-01") $ secondsToDiffTime 0
    home = SitemapUrl HomeR date Daily 1.0
    screencasts = SitemapUrl ScreencastsR date Daily 0.9
    examples = SitemapUrl ExamplesR date Daily 0.9
    book = SitemapUrl BookR date Daily 0.9
    go1 (Chapter { chapterSlug = name, chapterTitle = title}) =
        SitemapUrl (ChapterR $ T.unpack name) date Weekly 0.8
    go2 e = SitemapUrl
                (EntryR $ entrySlug e)
                (UTCTime (entryDay e) $ secondsToDiffTime 0)
                Monthly 0.5

getFeedR = do
    y <- getYesod
    let uday = entryDay $ head $ getEntries y
    atomFeed AtomFeed
        { atomTitle = "Yesob Web Framework"
        , atomLinkSelf = FeedR
        , atomLinkHome = HomeR
        , atomUpdated = UTCTime uday $ secondsToDiffTime 0
        , atomEntries = map go $ take 10 $ getEntries y
        }
  where
    go e = AtomFeedEntry
        { atomEntryLink = EntryR $ entrySlug e
        , atomEntryUpdated = UTCTime (entryDay e) $ secondsToDiffTime 0
        , atomEntryTitle = entryTitle e
        , atomEntryContent = entryContent e
        }

withYesodDocs f = do
    entries <- loadEntries
    let static = fileLookupDir "static" typeByExt
    book <- loadBook
    app <- toWaiApp $ YesodDocs static entries book
    f app

chapterToHtml :: Chapter -> Html
chapterToHtml (Chapter { chapterIntro = intro, chapterSections = sections }) = [$hamlet|
$forall intro b
    $blockToHtml.b$
$forall sections s
    %h2 $sectionTitle.s$
    $forall sectionBlocks.s b
        $blockToHtml.b$
|]

blockToHtml :: Block -> Html
blockToHtml (Paragraph is) = [$hamlet|
%p
    $forall is i
        $inlineToHtml.i$
|]
blockToHtml (UList items) = [$hamlet|
%ul
    $forall items i
        $listItemToHtml.i$
|]

listItemToHtml :: ListItem -> Html
listItemToHtml (ListItem is) = [$hamlet|
%li
    $forall is i
        $inlineToHtml.i$
|]

inlineToHtml :: Inline -> Html
inlineToHtml (Inline t) = [$hamlet|$t$|]
inlineToHtml (Emphasis is) = [$hamlet|
%i
    $forall is i
        $inlineToHtml.i$
|]
inlineToHtml (Term t) = [$hamlet|%b $t$|]
inlineToHtml (Hackage t) = [$hamlet|
%a!href="http://hackage.haskell.org/package/$t$" $t$
|]
