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
import qualified Text.Highlighting.Kate as Kate
import Text.XHtml.Strict (showHtmlFragment)
import Data.Either (lefts)

data YesodDocs = YesodDocs
    { getStatic :: Static
    , getEntries :: [Entry]
    , getBook :: Book
    }

type Handler = GHandler YesodDocs YesodDocs

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

navLinks :: [(String, Either String YesodDocsRoute)]
navLinks =
    [ ("Home", Right HomeR)
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

getHomeR :: Handler RepHtml
getHomeR = defaultLayout $ do
    setTitle "Yesod Web Framework for Haskell"
    addHamlet $(hamletFile "root")
    addHtmlHead [$hamlet|
%meta!name=description!value="Yesod Web Framework for Haskell. Create RESTful web apps with type safety."
|]
    addCassius $(cassiusFile "root")

getFiveMinutesR :: Handler RepHtml
getFiveMinutesR = defaultLayout $ do
    setTitle "Yesod in Five Minutes"
    addHamlet $(hamletFile "five-minutes")

getBookR :: Handler RepHtml
getBookR = defaultLayout $ do
    setTitle "Yesod Web Framework Book"
    book <- liftHandler $ fmap getBook getYesod
    let unpack = T.unpack
    let chapters = concatMap partChapters $ bookParts book
    addHamlet $(hamletFile "book")
    addCassius $(cassiusFile "book")

getChapterR :: String -> Handler RepHtml
getChapterR slug = do
    book <- fmap getBook getYesod
    let chapters = concatMap partChapters $ bookParts book
    chapter <-
        case filter (\x -> chapterSlug x == T.pack slug) chapters of
            [] -> notFound
            x:_ -> return x
    let previous = getPrev chapters
    let next = getNext chapters
    y <- getYesod
    let title = T.unpack $ chapterTitle chapter
    let unpack = T.unpack
    let html = chapterToHtml chapter
    defaultLayout $ do
        setTitle $ string $ "Yesod Book: " ++ title
        addScriptEither $ urlJqueryJs y
        addHamlet $(hamletFile "chapter")
        addCassius $(cassiusFile "book")
        addStylesheet $ StaticR hk_kate_css
        addJulius $(juliusFile "chapter")
        addCassius $(cassiusFile "chapter")
        addStylesheet $ StaticR hscolour_css
        -- addScript $ StaticR hyphenate_js
  where
    slug' = T.pack slug
    getPrev (x:yc@(Chapter { chapterSlug = y }):rest)
        | y == slug' = Just x
        | otherwise = getPrev $ yc : rest
    getPrev _ = Nothing
    getNext ((Chapter { chapterSlug = x }):y:rest)
        | x == slug' = Just y
        | otherwise = getNext $ y : rest
    getNext _ = Nothing
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
        , show (i :: Int)
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

getExamplesR :: Handler RepHtml
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

colorize :: String -> String -> Bool -> String -- FIXME use kate instead!
colorize title raw isLit = hscolour CSS defaultColourPrefs True False title isLit raw

getExampleR :: String -> Handler RepHtml
getExampleR name = do
    title <- maybe notFound return $ lookup name examples
    raw <- liftIO $ U.readFile $ "yesod/tutorial/" ++ name ++ ".lhs"
    let html = colorize title raw True
    return $ RepHtml $ toContent html

getScreencastsR :: Handler RepHtml
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

getArticlesR :: Handler RepHtml
getArticlesR = defaultLayout $ do
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

getSynWrqR :: Handler RepHtml
getSynWrqR = do
    let title = "web-routes-quasi Synopsis"
    raw <- liftIO $ U.readFile "synopsis/web-routes-quasi.markdown"
    let pandoc = readMarkdown defaultParserState raw
    let content = preEscapedString $ writeHtmlString defaultWriterOptions pandoc
    defaultLayout $ do
        setTitle title
        addHamlet $(hamletFile "synopsis")

synLhs :: String -> String -> Handler RepHtml
synLhs file title' = do
    let title = title' ++ " Synopsis"
    raw <- liftIO $ U.readFile $ "synopsis/" ++ file ++ ".lhs"
    let content = preEscapedString $ colorize title raw True
    defaultLayout $ do
        setTitle $ string title
        addStylesheet $ StaticR hscolour_css
        addHamlet $(hamletFile "synopsis")

getSynPerR :: Handler RepHtml
getSynPerR = synLhs "persistent" "Persistent"

getSynHamR :: Handler RepHtml
getSynHamR = synLhs "hamlet" "Hamlet"

getBlogR :: Handler ()
getBlogR = do
    y <- getYesod
    redirect RedirectTemporary $ EntryR $ entrySlug $ head $ getEntries y

getEntryR :: String -> Handler RepHtml
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

getFaviconR :: Handler ()
getFaviconR = sendFile "image/x-icon" "favicon.ico" >> return ()

getRobotsR :: Handler RepPlain
getRobotsR = robots SitemapR

getSitemapR :: Handler RepXml
getSitemapR = do
    y <- getYesod
    sitemap $ home : screencasts : examples' : book
            : map go1 (concatMap partChapters $ bookParts $ getBook y)
            ++ map go2 (getEntries y)
  where
    date = UTCTime (read "2010-09-01") $ secondsToDiffTime 0
    home = SitemapUrl HomeR date Daily 1.0
    screencasts = SitemapUrl ScreencastsR date Daily 0.9
    examples' = SitemapUrl ExamplesR date Daily 0.9
    book = SitemapUrl BookR date Daily 0.9
    go1 (Chapter { chapterSlug = name }) =
        SitemapUrl (ChapterR $ T.unpack name) date Weekly 0.8
    go2 e = SitemapUrl
                (EntryR $ entrySlug e)
                (UTCTime (entryDay e) $ secondsToDiffTime 0)
                Monthly 0.5

getFeedR :: Handler RepAtom
getFeedR = do
    y <- getYesod
    let uday = entryDay $ head $ getEntries y
    atomFeed AtomFeed
        { atomTitle = "Yesod Web Framework"
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

withYesodDocs :: (Application -> IO a) -> IO a
withYesodDocs f = do
    entries <- loadEntries
    let static = fileLookupDir "static" typeByExt
    book <- loadBook
    app <- toWaiApp $ YesodDocs static entries book
    f app

chapterToHtml :: Chapter -> Hamlet YesodDocsRoute
chapterToHtml (Chapter { chapterIntro = intro, chapterSections = sections
                       , chapterSummary = msummary }) = [$hamlet|
#toc
    %ul
        %li
            %a!href="#introduction" Introduction
        $forall sections s
            ^sectionToc.s^
        $maybe msummary _
            %li
                %a!href="#summary" Summary
#introduction
    $forall intro b
        ^blockToHtml.b^
$forall sections s
    %h2#$sectionId.s$ $sectionTitle.s$
    $forall sectionBlocks.s b
        ^(sectionBlockToHtml.firstLevel).b^
$maybe msummary summary
    %h2#summary Summary
    $forall summary b
        ^blockToHtml.b^
|]
  where
    firstLevel = 3

sectionToc :: Section -> Hamlet YesodDocsRoute
sectionToc s = [$hamlet|
%li
    %a!href="#$sectionId.s$" $sectionTitle.s$
    $if not.null.lefts.sectionBlocks.s
        %ul
            $forall lefts.sectionBlocks.s s
                ^sectionToc.s^
|]

sectionBlockToHtml :: Int -> Either Section Block -> Hamlet YesodDocsRoute
sectionBlockToHtml level (Left section) = [$hamlet|
!class=section$show.level$
    ^((showTitle.(sectionId.section)).level).sectionTitle.section^
    $forall sectionBlocks.section b
        ^(sectionBlockToHtml.nextLevel).b^
|]
  where
    showTitle :: T.Text -> Int -> T.Text -> Hamlet YesodDocsRoute
    showTitle i 3 t = [$hamlet|%h3#$i$ $t$|]
    showTitle i 4 t = [$hamlet|%h4#$i$ $t$|]
    showTitle i 5 t = [$hamlet|%h5#$i$ $t$|]
    showTitle i _ t = [$hamlet|%h6#$i$ $t$|]
    nextLevel = level + 1
sectionBlockToHtml _ (Right b) = blockToHtml b

blockToHtml :: Block -> Hamlet YesodDocsRoute -- FIXME put one of those paragraph symbol links here
blockToHtml (Paragraph pid is) = [$hamlet|
%p#$pid$
    $forall is i
        ^inlineToHtml.i^
|]
blockToHtml (Note is) = [$hamlet|
%p.note
    Note: $
    $forall is i
        ^inlineToHtml.i^
|]
blockToHtml (UList items) = [$hamlet|
%ul
    $forall items i
        ^listItemToHtml.i^
|]
blockToHtml (OList items) = [$hamlet|
%ol
    $forall items i
        ^listItemToHtml.i^
|]
blockToHtml (CodeBlock c) = [$hamlet|
%code
    %pre $c$
|]
blockToHtml (Snippet s) = [$hamlet|
$preEscapedString.showHtmlFragment.html$
|]
  where
    html = Kate.formatAsXHtml [Kate.OptNumberLines] "haskell" s
blockToHtml (Advanced bs) = [$hamlet|
.advanced
    $forall bs b
        ^blockToHtml.b^
|]
blockToHtml (Image { imageSrc = src, imageTitle = title }) = [$hamlet|
.image
    %img!src="/static/book/$src$"!alt=$title$!title=$title$
    $title$
|]
blockToHtml (Defs ds) = [$hamlet|
%table!border=1
    $forall ds d
        %tr
            %th $fst.d$
            %td
                $forall snd.d i
                    ^inlineToHtml.i^
|]
blockToHtml (Markdown text) =
    [$hamlet|$content$|]
  where
    pandoc = readMarkdown defaultParserState $ T.unpack text
    content = preEscapedString $ writeHtmlString defaultWriterOptions pandoc

listItemToHtml :: ListItem -> Hamlet YesodDocsRoute
listItemToHtml (ListItem is) = [$hamlet|
%li
    $forall is i
        ^inlineToHtml.i^
|]

inlineToHtml :: Inline -> Hamlet YesodDocsRoute
inlineToHtml (Inline t) = [$hamlet|$t$|]
inlineToHtml (Emphasis is) = [$hamlet|
%i
    $forall is i
        ^inlineToHtml.i^
|]
inlineToHtml (Term t) = [$hamlet|%b $t$|]
inlineToHtml (Hackage t) = [$hamlet|
%a!href="http://hackage.haskell.org/package/$t$" $t$
|]
inlineToHtml (Xref href inner) = [$hamlet|%a!href=$href$ $inner$|]
inlineToHtml (Code inner) = [$hamlet|%code $inner$|]
inlineToHtml (Link chapter msection inner) = [$hamlet|
%a!href="@ChapterR.unpack.chapter@$section$" $inner$
|]
  where
    section =
        case msection of
            Nothing -> ""
            Just section' -> [$hamlet|\#$section'$|] :: Html
    unpack = T.unpack
inlineToHtml (Abbr title inner) = [$hamlet|
%abbr!title=$title$ $inner$
|]
