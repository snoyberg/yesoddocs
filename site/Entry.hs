module Entry
    ( Entry (..)
    , loadEntries
    ) where

import qualified Data.ByteString as S
import Data.ByteString.UTF8 (toString)
import System.Directory
import System.IO
import Data.List
import Control.Monad
import Data.Time
import System.Locale
import Text.Hamlet
import Data.Ord (comparing)
import Text.Pandoc
import qualified Text.XML.Enumerator.Document as D
import Text.XML.Enumerator.Parse (decodeEntities)
import qualified Data.XML.Types as XML
import Dita (renderDita)

data EntryFormat = EFHtml String | EFMarkdown String | EFDita XML.Document

loadEntry :: String -> IO Entry
loadEntry slug = do
    let fp = "blog/" ++ slug
    withFile fp ReadMode $ \h -> do
        firstLine <- toString `fmap` S.hGetLine h
        format <-
            case firstLine of
                "!markdown" -> do
                    title <- toString `fmap` S.hGetLine h
                    return $ EFMarkdown title
                "!dita" -> do
                    doc <- D.readFile_ ("../dita/" ++ slug ++ ".dita") decodeEntities
                    return $ EFDita doc
                '!':x -> error $ "Unknown first line: " ++ x
                _ -> return $ EFHtml firstLine
        date' <- fmap toString $ S.hGetLine h
        date <- case reads date' of
                    (d, _):_ -> return d
                    _ -> error $ "Invalid date for " ++ slug ++ ": " ++ date'
        contents <- S.hGetContents h
        let (html, title) =
                case format of
                    EFHtml title' -> (unsafeByteString contents, title')
                    EFMarkdown title'
                        -> (preEscapedString
                         $ writeHtmlString defaultWriterOptions
                         $ readMarkdown defaultParserState
                         $ toString contents, title')
                    EFDita doc -> renderDita doc
        return Entry
            { entrySlug = slug
            , entryTitle = title
            , entryDate = showDay date
            , entryDay = date
            , entryContent = html
            , entryYearMonth = toYearMonth date
            }
  where
    toYearMonth = formatTime defaultTimeLocale "%B %Y"
    showDay = formatTime defaultTimeLocale "%B %e, %Y"

data Entry = Entry
    { entrySlug :: String
    , entryTitle :: String
    , entryDate :: String
    , entryDay :: Day
    , entryYearMonth :: String
    , entryContent :: Html
    }

notHidden :: String -> Bool
notHidden ('.':_) = False
notHidden _ = True

loadEntries :: IO [Entry]
loadEntries = fmap (reverse . sortBy (comparing entryDay)) $ do
    let top = "blog/"
    allContents <- getDirectoryContents top
    allFiles <- filterM (\f -> doesFileExist $ top ++ f)
              $ filter notHidden allContents
    mapM loadEntry allFiles
