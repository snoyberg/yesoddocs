{-# LANGUAGE TypeFamilies, QuasiQuotes, TemplateHaskell, OverloadedStrings, MultiParamTypeClasses #-}
module Handler.Blog where

import Yesod
import Yesod.Helpers.AtomFeed
import Yesod.Form.Jquery
import Settings
import Entry
import Control.Arrow ((&&&))
import Data.Function (on)
import Data.Time
import YesodDocs
import Data.List (groupBy)
import Data.Text (pack)

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
        setTitle $ toHtml $ "Yesod Blog: " ++ entryTitle entry
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
        , feedEntryTitle = pack $ entryTitle e
        , feedEntryContent = entryContent e
        }
