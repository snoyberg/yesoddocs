module Entry where

import qualified Data.ByteString as S
import Data.ByteString.UTF8 (toString)
import System.Directory
import System.IO
import Data.Serialize
import Control.Arrow
import Data.List
import Data.Function
import Control.Monad
import Control.Applicative
import Data.Time
import System.Locale
import Text.Hamlet
import Data.Ord (comparing)

loadEntry :: String -> IO Entry
loadEntry slug = do
    let fp = "blog/" ++ slug
    withFile fp ReadMode $ \h -> do
        title <- S.hGetLine h
        date' <- S.hGetLine h
        let date = read $ toString date' :: Day
        contents <- S.hGetContents h
        return Entry
            { entrySlug = slug
            , entryTitle = toString title
            , entryDate = showDay date
            , entryDay = date
            , entryContent = unsafeByteString contents
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
    deriving Show

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
