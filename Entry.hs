module Entry
    ( Entry (..)
    , loadEntry
    ) where

import qualified Data.ByteString.Char8 as S
import Data.Time (Day)
import Data.Text (unpack, pack, Text)
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Model (TopicFormat (..))

toString :: S.ByteString -> String
toString = unpack . decodeUtf8With lenientDecode

data EntryFormat = EFHtml String | EFMarkdown String
    deriving Show

loadEntry :: FilePath -> S.ByteString -> IO Entry
loadEntry file content = do
    putStrLn $ "Loading " ++ file
    let takeLine s =
            let (x, y) = S.break (== '\n') s
             in (toString $ S.takeWhile (/= '\r') x, S.drop 1 y)
    let (firstLine, content') = takeLine content
    (format, body) <-
        case firstLine of
            "!markdown" -> do
                let (title, body) = takeLine content'
                return (EFMarkdown title, body)
            '!':x -> error $ "Unknown first line: " ++ x
            _ -> return (EFHtml firstLine, content')
    print (format, content')
    let (date', body') = takeLine body
    date <- case reads date' of
                (d, _):_ -> return d
                _ -> error $ "Invalid date for " ++ file ++ ": " ++ date'
    let (format', title) =
            case format of
                EFHtml title' -> (TFHtml, title')
                EFMarkdown title' -> (TFMarkdown, title')
    return Entry
        { entryTitle = pack title
        , entryDay = date
        , entryContent = pack $ toString body'
        , entryFormat = format'
        }

data Entry = Entry
    { entryTitle :: Text
    , entryDay :: Day
    , entryContent :: Text
    , entryFormat :: TopicFormat
    }
