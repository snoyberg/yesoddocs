{-# LANGUAGE TypeFamilies, QuasiQuotes, TemplateHaskell, OverloadedStrings, MultiParamTypeClasses #-}
{-# LANGUAGE CPP #-}

module YesodDocs where

import Yesod
import Yesod.Helpers.Static
import Yesod.Helpers.AtomFeed
import Yesod.Form.Jquery
import Settings
import Language.Haskell.HsColour hiding (string)
import Language.Haskell.HsColour.Colourise (defaultColourPrefs)
import Entry
import Data.Time
import Book
import qualified Data.Text as T
import Data.Text (Text)
import qualified Text.Highlighting.Kate as Kate
import Text.XHtml.Strict (showHtmlFragment)
import Control.Concurrent.AdvSTM
import Control.Concurrent.AdvSTM.TVar
import Comments
import Data.List (sortBy)
import Text.Blaze (toHtml)
import qualified Text.XHtml

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

mkYesodData "YesodDocs" [parseRoutes|
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
/community CommunityR GET

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
    approot _ = "http://www.yesodweb.com"
    defaultLayout widget = do
        setHeader "Cache-Control" "max-age=3600, public"
        curr <- getCurrentRoute
        tm <- getRouteToMaster
        --let isCurrent x = fmap (Right . tm) curr == Just x
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
        let isHome = fmap tm curr == Just HomeR
        (title, bcs) <- breadcrumbs
        hamletToRepHtml $(hamletFile "default-layout")
      where
        addGoogleFont s = addStylesheetRemote $ T.pack $ "http://fonts.googleapis.com/css?family=" ++ s

instance YesodBreadcrumbs YesodDocs where
    breadcrumb HomeR = return ("Home", Nothing)
    breadcrumb FiveMinutesR = return ("Yesod in Five Minutes", Just HomeR)
    breadcrumb BookR = return ("Book", Just HomeR)
    breadcrumb ScreencastsR = return ("Screencasts", Just HomeR)
    breadcrumb AboutR = return ("About", Just HomeR)
    breadcrumb CommunityR = return ("Community", Just HomeR)
    breadcrumb (EntryR slug) = do
        y <- getYesod
        entry <-
            case filter (\x -> entrySlug x == slug) $ getEntries y of
                [] -> notFound
                e:_ -> return e
        return (entryTitle entry, Just HomeR)
    breadcrumb (ChapterR slug) = do
        book <- fmap getBook getYesod
        let chapters = concatMap partChapters $ bookParts book
        chapter <-
            case filter (\x -> chapterSlug x == T.pack slug) chapters of
                [] -> notFound
                x:_ -> return x
        return (T.unpack $ chapterTitle chapter, Just BookR)

    breadcrumb FaviconR = return ("", Nothing)
    breadcrumb RobotsR = return ("", Nothing)
    breadcrumb SitemapR = return ("", Nothing)
    breadcrumb FeedR = return ("", Nothing)
    breadcrumb StaticR{} = return ("", Nothing)
    breadcrumb ArticlesR = return ("", Nothing)
    breadcrumb ExamplesR = return ("", Nothing)
    breadcrumb SynWrqR = return ("", Nothing)
    breadcrumb SynPerR = return ("", Nothing)
    breadcrumb SynHamR = return ("", Nothing)
    breadcrumb BlogR = return ("", Nothing)
    breadcrumb CommentR{} = return ("", Nothing)
    breadcrumb CommentsFeedR = return ("", Nothing)
    breadcrumb OneCommentR{} = return ("", Nothing)

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

colorize :: String -> String -> Bool -> String -- FIXME use kate instead!
colorize title raw isLit = hscolour CSS defaultColourPrefs True False title isLit raw

postCommentR :: String -> Text -> Handler ()
postCommentR slug pid = do
    name <- runFormPost' $ stringInput "name" -- FIXME textInput
    content <- runFormPost' $ stringInput "content" -- FIXME textareaInput
    _ <- runFormPost' $ stringInput "code"
    now <- liftIO getCurrentTime
    let cm = Comment name (Textarea content) now
    tcs <- fmap comments getYesod
    liftIO $ atomically $ do
        cs <- readTVar tcs
        let cs' = addComment cm cs
        writeTVar tcs cs'
        onCommit $ saveComments cs'
    setMessage "Your comment has been submitted"
    r <- getUrlRender
    redirectText RedirectTemporary $ T.concat
        [ r (ChapterR slug)
        , "#"
        , pid
        ]
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
                let dest = T.unpack (r (ChapterR chapter)) ++ '#' : T.unpack para
                redirectText RedirectPermanent $ T.pack dest
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

formatKateLines :: [Kate.SourceLine] -> Text.XHtml.Html
formatKateLines = Kate.formatAsXHtml [Kate.OptNumberLines] "haskell"
