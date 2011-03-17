{-# LANGUAGE TypeFamilies, QuasiQuotes, TemplateHaskell, OverloadedStrings, MultiParamTypeClasses #-}
{-# LANGUAGE CPP #-}

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
import qualified Data.Text as T
import qualified Data.Text as TS -- FIXME remove
import Data.Text (Text)
import qualified Text.Highlighting.Kate as Kate
import Text.XHtml.Strict (showHtmlFragment)
import Data.Either (lefts)
import Control.Concurrent.AdvSTM
import Control.Concurrent.AdvSTM.TVar
import Comments
import Data.List (sortBy)
import Text.Blaze (toHtml)
import qualified Data.ByteString.Char8 as S8

#ifndef PRODUCTION
import Debug.Trace
debug :: (Show a) => a -> a
debug a = trace (show a) a
#endif

data YesodDocs = YesodDocs
    { getStatic :: Static
    , getEntries :: [Entry]
    , getBook :: Book
    , comments :: TVar Comments
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

/screencasts ScreencastsR GET

/synopsis/web-routes-quasi SynWrqR GET
/synopsis/persistent SynPerR GET
/synopsis/hamlet SynHamR GET

/blog BlogR GET
/blog/#String EntryR GET

/about AboutR GET

/comment/#String/#Text CommentR POST
/feed/comments CommentsFeedR GET
/comment/#String OneCommentR GET
|]

navLinks :: [(String, Either String YesodDocsRoute)]
navLinks =
    [ ("Blog", Right BlogR)
    , ("About", Right AboutR)
    , ("Yesod in 5 Minutes", Right FiveMinutesR)
    , ("Book", Right BookR)
    , ("Screencasts", Right ScreencastsR)
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
            addGoogleFont "Cantarell"
            addGoogleFont "Inconsolata"
            addGoogleFont "Droid+Serif"
            addCassius $(cassiusFile "default-layout")
            atomLink FeedR "Yesod Blog"
            widget
        hamletToRepHtml $(hamletFile "default-layout")
      where
        addGoogleFont s = addStylesheetRemote $ "http://fonts.googleapis.com/css?family=" ++ s

instance YesodJquery YesodDocs where
    urlJqueryJs _ = Left $ StaticR jquery_js
    urlJqueryUiJs _ = Left $ StaticR jquery_ui_js
    urlJqueryUiCss  _ = Left $ StaticR jquery_ui_custom_css



highlightedHamletExample :: Html
highlightedHamletExample =
    case Kate.highlightAs "html" hamletExample of
        Left e -> error $ "Could not parse front page hamlet snippet, error: " ++ e
        Right parsedLines -> preEscapedString $ showHtmlFragment $ Kate.formatAsXHtml [] "html" parsedLines

hamletExample :: String
hamletExample = concatMap (++ "\n") [
    "<table#yesod-id.docs-class>",
    "  <thead>",
    "    <tr>",
    "      <th>name",
    "      <th>features",
    "  <tbody data-superflous=none>",
    "    <tr>",
    "      <td>",
    "        <a href=@{DocR doc}>#{docName doc}",
    "      <td>#{docFeature f}"
  ]

getHomeR :: Handler RepHtml
getHomeR = defaultLayout $ do
    setTitle "Yesod Web Framework for Haskell"
    let faqR = ChapterR "faq"
    addHamlet $(hamletFile "root")
    addHtmlHead [$hamlet|\
<meta name="description" content="Yesod Web Framework for Haskell. Create RESTful web apps with type safety.">
|]
    addCassius $(cassiusFile "root")
    addStylesheet $ StaticR hk_html_css
    y <- lift getYesod
    addScriptEither $ urlJqueryJs y
    addScriptRemote "http://cdn.jquerytools.org/1.2.0/full/jquery.tools.min.js"
    addJulius $(juliusFile "root")

getFiveMinutesR :: Handler RepHtml
getFiveMinutesR = defaultLayout $ do
    setTitle "Yesod in Five Minutes"
    addCassius $(cassiusFile "five-minutes")
    addHamlet $(hamletFile "five-minutes")

getAboutR :: Handler RepHtml
getAboutR = defaultLayout $ do
    setTitle "About Yesod"
    addCassius $(cassiusFile "about")
    addHamlet $(hamletFile "about")

getBookR :: Handler RepHtml
getBookR = defaultLayout $ do
    setTitle "Yesod Web Framework Book"
    book <- lift $ fmap getBook getYesod
    let unpack = T.unpack
    let chapters = concatMap partChapters $ bookParts book
    atomLink CommentsFeedR "Book Comments"
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
    tcs <- fmap comments getYesod
    cs' <- liftIO $ atomically $ readTVar tcs
    let cs = fromMaybe [] $ lookup slug cs'
    let html = chapterToHtml cs chapter
    defaultLayout $ do
        setTitle $ string $ "Yesod Book: " ++ title
        addScriptEither $ urlJqueryJs y
        atomLink CommentsFeedR "Book Comments"
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

getExamplesR :: Handler RepHtml
getExamplesR = defaultLayout [$hamlet|\
<h1>We've Moved!
<p>
    \The examples have now been merged into the 
    <a href="@{BookR}">Yesod book
    \. Each example is its own chapter in the Examples part. Enjoy!
|]

colorize :: String -> String -> Bool -> String -- FIXME use kate instead!
colorize title raw isLit = hscolour CSS defaultColourPrefs True False title isLit raw

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

getArticlesR :: Handler ()
getArticlesR = redirect RedirectPermanent $ ChapterR "blog-posts"

getSynWrqR :: Handler RepHtml
getSynWrqR = do
    let title = "web-routes-quasi Synopsis"
    raw <- liftIO $ U.readFile "yesod-examples/synopsis/web-routes-quasi.markdown"
    let pandoc = readMarkdown defaultParserState raw
    let content = preEscapedString $ writeHtmlString defaultWriterOptions pandoc
    defaultLayout $ do
        setTitle title
        addHamlet $(hamletFile "synopsis")

synLhs :: String -> String -> Handler RepHtml
synLhs file title' = do
    let title = title' ++ " Synopsis"
    raw <- liftIO $ U.readFile $ "yesod-examples/synopsis/" ++ file ++ ".lhs"
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
    atomFeed Feed
        { feedTitle = "Yesod Web Framework"
        , feedLinkSelf = FeedR
        , feedLinkHome = HomeR
        , feedUpdated = UTCTime uday $ secondsToDiffTime 0
        , feedEntries = map go $ take 10 $ getEntries y
        , feedLanguage = "en"
        , feedDescription = "Yesod Web Framework"
        }
  where
    go e = FeedEntry
        { feedEntryLink = EntryR $ entrySlug e
        , feedEntryUpdated = UTCTime (entryDay e) $ secondsToDiffTime 0
        , feedEntryTitle = entryTitle e
        , feedEntryContent = entryContent e
        }

withYesodDocs :: (Application -> IO a) -> IO a
withYesodDocs f = do
    entries <- loadEntries
    let s = static "static"
    book <- loadBook
    cs <- loadComments
    tcomments <- newTVarIO cs
    app <- toWaiApp $ YesodDocs s entries book tcomments
    f app

chapterToHtml :: [(Text, [Comment])] -> Chapter -> Hamlet YesodDocsRoute
chapterToHtml cs c@(Chapter { chapterIntro = intro, chapterSections = sections
                       , chapterSummary = msummary }) = [$hamlet|\
$if not (null sections)
    <div id="toc">
        <ul>
            <li>
                <a href="#introduction">Introduction
            $forall s <- sections
                \^{sectionToc s}
            $maybe _ <- msummary
                <li>
                    <a href="#summary">Summary
<div id="introduction">
    $forall b <- intro
        \^{blockToHtml c cs b}
$forall s <- sections
    <h2 id="#{sectionId s}">#{sectionTitle s}
    $forall b <- sectionBlocks s
        \^{sectionBlockToHtml c cs firstLevel b}
$maybe summary <- msummary
    <h2 id="summary">Summary
    $forall b <- summary
        \^{blockToHtml c cs b}
|]
  where
    firstLevel = 3

sectionToc :: Section -> Hamlet YesodDocsRoute
sectionToc s = [$hamlet|\
<li>
    <a href="##{sectionId s}">#{sectionTitle s}
    $if not (null (lefts (sectionBlocks s)))
        <ul>
            $forall s <- lefts (sectionBlocks s)
                \^{sectionToc s}
|]

sectionBlockToHtml :: Chapter -> [(Text, [Comment])]
                   -> Int -> Either Section Block -> Hamlet YesodDocsRoute
sectionBlockToHtml chap cs level (Left section) = [$hamlet|\
<div class="section#{show level}">
    \^{showTitle (sectionId section) level (sectionTitle section)}
    $forall b <- sectionBlocks section
        \^{sectionBlockToHtml chap cs nextLevel b}
|]
  where
    showTitle :: T.Text -> Int -> T.Text -> Hamlet YesodDocsRoute
    showTitle i 3 t = [$hamlet|<h3 id="#{i}">#{t}
|]
    showTitle i 4 t = [$hamlet|<h4 id="#{i}">#{t}
|]
    showTitle i 5 t = [$hamlet|<h5 id="#{i}">#{t}
|]
    showTitle i _ t = [$hamlet|<h6 id="#{i}">#{t}
|]
    nextLevel = level + 1
sectionBlockToHtml chap cs _ (Right b) = blockToHtml chap cs b

blockToHtml :: Chapter -> [(Text, [Comment])] -> Block -> Hamlet YesodDocsRoute -- FIXME put one of those paragraph symbol links here
blockToHtml chap chapCs (Paragraph pid is) = [$hamlet|
<p id="#{pid}">
    <span .permalink
        <a href=##{pid}>&#x204B
    $forall i <- is
        \^{inlineToHtml i}
    <span .comment-count>#{csCount'}
<div .comments>
    $forall c <- cs
        <div .comment>
            <span .name>#{commentName c}
            <span .datetime>#{show (utctDay (commentTime c))}
            <div .content>#{commentContent c}
    <form method="post" action="@{CommentR (TS.unpack (chapterSlug chap)) $ TS.pack $ T.unpack pid}">
        <b>Add a comment
        \Name: 
        <input type="text" name="name" .name>
        <textarea name="content">
        <input type="submit" value="Add comment">
|]
  where
    unpack = TS.unpack
    cs = fromMaybe [] $ lookup (T.pack $ TS.unpack pid) chapCs
    csCount = length cs
    csCount'
        | csCount == 0 = "No comments"
        | csCount == 1 = "1 comment"
        | otherwise = show csCount ++ " comments"
blockToHtml _ _ (Note is) = [$hamlet|\
<p .note>
    \Note: 
    $forall i <- is
        \^{inlineToHtml i}
|]
blockToHtml _ _ (UList items) = [$hamlet|\
<ul>
    $forall i <- items
        \^{listItemToHtml i}
|]
blockToHtml _ _ (OList items) = [$hamlet|\
<ol>
    $forall i <- items
        \^{listItemToHtml i}
|]
blockToHtml _ _ (CodeBlock c) = [$hamlet|\
<code>
    <pre>#{c}
|]
blockToHtml _ _ (Snippet s) = [$hamlet|\
\#{preEscapedString (showHtmlFragment html)}
|]
  where
    html = Kate.formatAsXHtml [Kate.OptNumberLines] "haskell" s
blockToHtml chap cs (Advanced bs) = [$hamlet|\
<div .advanced>
    $forall b <- bs
        \^{blockToHtml chap cs b}
|]
blockToHtml _ _ (Image { imageSrc = src, imageTitle = title }) = [$hamlet|\
<div .image>
    <img src="/static/book/#{src}" alt="#{title}" title="#{title}">
    \#{title}
|]
blockToHtml _ _ (Defs ds) = [$hamlet|\
<table border="1">
    $forall d <- ds
        <tr>
            <th>#{fst d}
            <td>
                $forall i <- snd d
                    \^{inlineToHtml i}
|]
blockToHtml _ _ (Markdown text) =
    [$hamlet|\#{content}
|]
  where
    pandoc = readMarkdown defaultParserState $ T.unpack text
    content = preEscapedString $ writeHtmlString defaultWriterOptions pandoc
blockToHtml _ _ (Example text) =
    const $ preEscapedString $ colorize "" (T.unpack text) True

listItemToHtml :: ListItem -> Hamlet YesodDocsRoute
listItemToHtml (ListItem is) = [$hamlet|\
<li>
    $forall i <- is
        \^{inlineToHtml i}
|]

inlineToHtml :: Inline -> Hamlet YesodDocsRoute
inlineToHtml (Inline t) = [$hamlet|\#{t}
|]
inlineToHtml (Emphasis is) = [$hamlet|\
<i>
    $forall i <- is
        \^{inlineToHtml i}
|]
inlineToHtml (Strong is) = [$hamlet|\
<b>
    $forall i <- is
        \^{inlineToHtml i}
|]
inlineToHtml (Term t) = [$hamlet|<b>#{t}
|]
inlineToHtml (Hackage t) = [$hamlet|\
<a href="http://hackage.haskell.org/package/#{t}">#{t}
|]
inlineToHtml (Xref href inner) = [$hamlet|<a href="#{href}">#{inner}
|]
inlineToHtml (Code inner) = [$hamlet|<code>#{inner}
|]
inlineToHtml (Link chapter msection inner) = [$hamlet|\
<a href="@{ChapterR (TS.unpack chapter)}#{section}">#{inner}
|]
  where
    section =
        case msection of
            Nothing -> ""
            Just section' -> [$hamlet|\##{section'}
|] :: Html
    unpack = T.unpack
inlineToHtml (Abbr title inner) = [$hamlet|\
<abbr title="#{title}">#{inner}
|]

postCommentR :: String -> Text -> Handler ()
postCommentR slug pid = do
    name <- runFormPost' $ stringInput "name" -- FIXME textInput
    content <- runFormPost' $ stringInput "content" -- FIXME textareaInput
    _ <- runFormPost' $ stringInput "code"
    now <- liftIO getCurrentTime
    let cm = Comment (T.pack name) (Textarea content) now
    tcs <- fmap comments getYesod
    liftIO $ atomically $ do
        cs <- readTVar tcs
        let cs' = addComment cm cs
        writeTVar tcs cs'
        onCommit $ saveComments cs'
    setMessage "Your comment has been submitted"
    r <- getUrlRender
    redirectString RedirectTemporary $ S8.pack $ r (ChapterR slug) ++ '#' : T.unpack pid
  where
    addComment cm ((slug', cs):rest)
        | slug == slug' = (slug', addComment' cm cs) : rest
        | otherwise = (slug', cs) : addComment cm rest
    addComment cm [] = [(slug, addComment' cm [])]
    addComment' cm ((pid', cs):rest)
        | pid == pid' = (pid', cs ++ [cm]) : rest
        | otherwise = (pid', cs) : addComment' cm rest
    addComment' cm [] = [(pid, [cm])]

getOneCommentR :: String -> Handler ()
getOneCommentR timeS = do
    time <-
        case reads $ map utos timeS of
            [] -> notFound
            (t, _):_ -> return t
    tcs <- fmap comments getYesod
    cs' <- liftIO $ atomically $ readTVar tcs
    mapM_ (go time) cs'
  where
    utos '_' = ' '
    utos 'c' = ':'
    utos c   = c
    go time (chapter, paras) =
        mapM_ (go' time) paras
      where
        go' time' (para, cs)
            | time' `elem` map commentTime cs = do
                r <- getUrlRender
                let dest = r (ChapterR chapter) ++ '#' : T.unpack para
                redirectString RedirectPermanent $ S8.pack dest
            | otherwise = return ()

getCommentsFeedR :: Handler RepAtom
getCommentsFeedR = do
    cacheSeconds 300
    tcs <- fmap comments getYesod
    cs' <- liftIO $ atomically $ readTVar tcs
    let cs = sortBy (\x y -> commentTime y `compare` commentTime x)
           $ concatMap (concatMap snd) $ map snd cs'
    atomFeed Feed
        { feedTitle = "Comments on the Yesod Web Framework book"
        , feedLinkSelf = CommentsFeedR
        , feedLinkHome = BookR
        , feedUpdated = commentTime $ head cs
        , feedEntries = map go $ take 10 cs
        , feedLanguage = "en"
        , feedDescription = "Comments on the Yesod Web Framework book"
        }
  where
    go c = FeedEntry
        { feedEntryLink = OneCommentR $ map stou $ show $ commentTime c
        , feedEntryUpdated = commentTime c
        , feedEntryTitle = "Comment by " ++ T.unpack (commentName c)
        , feedEntryContent = toHtml $ commentContent c
        }
    stou ' ' = '_'
    stou ':' = 'c'
    stou c   = c
