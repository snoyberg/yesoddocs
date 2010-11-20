{-# LANGUAGE DeriveDataTypeable #-}
module Book
    where

import Prelude hiding (head)
import Data.Text.Lazy (Text)
import Text.XML.LibXML.Enumerator
import Data.XML.Types
import Data.Enumerator
import Data.Enumerator.IO
import Data.Typeable (Typeable)
import Control.Exception (Exception)
import Control.Monad (unless)

data Book = Book
    { bookTitle :: Text
    , bookParts :: [Part]
    }

data Part = Part
    { partTitle :: Text
    , partChapters :: [Chapter]
    }

data Chapter = Chapter
    { chapterHref :: Text
    }

main = do
    run $ enumFile "book.xml" $$ joinI $ parseBytesIO Nothing $$ parseBook

data XmlException = XmlException String
    deriving (Show, Typeable)
instance Exception XmlException

require f = do
    y <- head
    case y of
        Nothing -> throwError $ XmlException "Unexpected end-of-stream"
        Just z ->
            case f z of
                Nothing -> 
    unless (Just x == y) $ throwError $ XmlException
        $ "Expected " ++ show start ++ ", received " ++ show y

between start end middle = do
    require start
    x <- middle
    require end
    return x

parseBook = between EventBeginDocument EventEndDocument $ do
    return "FIXME"
