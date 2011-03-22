{-# LANGUAGE TypeFamilies, QuasiQuotes, TemplateHaskell, OverloadedStrings, MultiParamTypeClasses #-}
module Handler.Blog where

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
import YesodDocs

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
