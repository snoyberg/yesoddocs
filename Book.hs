{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Book
    ( Book (..)
    , Inline (..)
    , Block (..)
    , ListItem (..)
    , Chapter (..)
    , Part (..)
    , Section (..)
    , loadBook
    ) where

import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import XmlParse
import Data.XML.Types
import Data.Enumerator (Iteratee, throwError)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Text.Hamlet (ToHtml (..), string)
import qualified Text.Highlighting.Kate as Kate
import qualified System.IO.UTF8 as U
import Data.List (group, sort)
import Data.String
import Control.Applicative ((<$>), (<*>))

data Book = Book
    { bookTitle :: Text
    , bookParts :: [Part]
    }
    deriving Show

data Part = Part
    { partTitle :: Text
    , partChapters :: [Chapter]
    }
    deriving Show

data ChapterStatus = Outline | Incomplete | Unproofed | Proofread | Finished
    deriving (Show, Read)
instance ToHtml ChapterStatus where toHtml = string . show

data Chapter = Chapter
    { chapterSlug :: Text
    , chapterTitle :: Text
    , chapterStatus :: ChapterStatus
    , chapterIntro :: [Block]
    , chapterSections :: [Section]
    , chapterSummary :: Maybe [Block]
    }
    deriving Show

data Section = Section
    { sectionId :: Text
    , sectionTitle :: Text
    , sectionBlocks :: [Either Section Block]
    }
    deriving Show

data Block = Paragraph { paraId :: Text, paraContents :: [Inline] }
           | UList [ListItem]
           | OList [ListItem]
           | CodeBlock Text
           | Snippet [Kate.SourceLine]
           | Advanced [Block]
           | Note [Inline]
           | Image { imageSrc :: Text, imageTitle :: Text }
           | Defs [(Text, [Inline])]
           | Markdown Text
    deriving Show

data Inline = Inline Text
            | Emphasis [Inline]
            | Term Text
            | Hackage Text
            | Xref { xrefHref :: Text, xrefInner :: Text }
            | Code Text
            | Link
                { linkChapter :: Text
                , linkSection :: Maybe Text
                , linkInner :: Text
                }
            | Abbr
                { abbrTitle :: Text
                , abbrInner :: Text
                }
    deriving Show

data ListItem = ListItem { unListItem :: [Inline] } -- FIXME block or inline
    deriving Show

loadBook :: IO Book
loadBook = parseFile_ "book.xml" (const Nothing) parseBook

parseBook :: MonadIO m => Iteratee SEvent m Book
parseBook = force "Missing book" $ tag'' "book" $ do
    title <- force "Book requires title" titleTag
    parts <- many $ tag'' "part" parsePart
    return $ Book title parts

titleTag :: Monad m => Iteratee SEvent m (Maybe Text)
titleTag = tag'' "title" content'

parsePart :: MonadIO m => Iteratee SEvent m Part
parsePart = do
    title <- force "Part requires title" titleTag
    chapters <- many $ tag' "chapter" (requireAttr "href") parseChapter'
    return $ Part title chapters

parseChapter' :: MonadIO m => Text -> Iteratee SEvent m Chapter
parseChapter' href' = do
    x <- liftIO $ parseFile (T.unpack href') (const Nothing) parseChapter
    case x of
        Left e -> throwError e
        Right c -> return c

parseChapter :: MonadIO m => Iteratee SEvent m Chapter
parseChapter = force "Missing chapter" $ tag' "chapter" ((,) <$> requireAttr "id" <*> requireAttr "status") $ \(slug, status') -> do
    status <- readStatus status'
    title <- force "Chapter requires title" titleTag
    intro <- force "Intro required" $ tag'' "intro" $ many parseBlock
    sections <- many $ tag' "section" (requireAttr "id") parseSection
    checkUniqueIds slug sections
    summary <- tag'' "summary" $ many parseBlock
    return $ Chapter slug title status intro sections summary
  where
    readStatus t =
        case reads $ T.unpack t of
            [] -> throwError $ XmlException ("Invalid status: " ++ show t) Nothing
            (s, _):_ -> return s
    checkUniqueIds slug =
        go slug . map head . filter (\x -> length x > 1) . group . sort . concatMap getIds
    go _ [] = return ()
    go slug x = throwError $ XmlException ("Chapter " ++ T.unpack slug ++ ", duplicated IDs: " ++ unwords (map T.unpack x)) Nothing
    getIds (Section id' _ blocks) = id' : concatMap getIds' blocks
    getIds' (Left s) = getIds s
    getIds' (Right (Paragraph id' _)) = [id']
    getIds' (Right _) = []

parseSection :: MonadIO m => Text -> Iteratee SEvent m Section
parseSection id' = do
    title <- force "Section requires title" titleTag
    blocks <- many parseBlockSection
    return $ Section id' title blocks

parseBlockSection :: MonadIO m => Iteratee SEvent m (Maybe (Either Section Block))
parseBlockSection = choose
    [ (fmap . fmap) Left $ tag' "section" (requireAttr "id") $ parseSection
    , (fmap . fmap) Right parseBlock
    ]

parseBlock :: MonadIO m => Iteratee SEvent m (Maybe Block)
parseBlock = choose
    [ tag' "p" (requireAttr "id") $ \id' -> do
        ins <- many parseInline
        return $ Paragraph id' ins
    , tag'' "ul" $ fmap UList $ many parseListItem
    , tag'' "ol" $ fmap OList $ many parseListItem
    , tag'' "codeblock" $ fmap CodeBlock content'
    , tag' "snippet" (requireAttr "name") parseSnippet
    , tag'' "advanced" $ fmap Advanced $ many parseBlock
    , tag'' "note" $ fmap Note $ many parseInline
    , tag' "image" (Image <$> requireAttr "src" <*> requireAttr "title") return
    , tag'' "defs" $ fmap Defs $ many $ tag' "def" (requireAttr "term") parseDef
    , tag'' "markdown" $ fmap Markdown content' -- FIXME this must be killed with fire
    ]

parseDef :: MonadIO m => Text -> Iteratee SEvent m (Text, [Inline])
parseDef term = do
    c <- many parseInline
    return (term, c)

parseSnippet :: MonadIO m => Text -> Iteratee SEvent m Block
parseSnippet name = do
    raw <- liftIO $ U.readFile $ "snippets/" ++ T.unpack name ++ ".hs" -- FIXME use text
    let raw' = unlines $ go False $ lines raw
    case Kate.highlightAs "haskell" raw' of
        Left e -> throwError $ XmlException (concat
            [ "Could not parse code snippet "
            , T.unpack name
            , ": "
            , e
            ]) Nothing
        Right lines' -> return $ Snippet lines'
  where
    go _ [] = []
    go False ("-- START":rest) = go True rest
    go True ("-- STOP":rest) = go False rest
    go False (_:rest) = go False rest
    go True (x:rest) = x : go True rest

parseInline :: MonadIO m => Iteratee SEvent m (Maybe Inline)
parseInline = choose
    [ (fmap . fmap) Inline content
    , tag'' "i" $ fmap Emphasis $ many parseInline
    , tag'' "term" $ fmap Term content'
    , tag'' "hackage" $ fmap Hackage content'
    , tag' "xref" (requireAttr "href") parseXref
    , tag'' "code" $ fmap Code content'
    , tag' "link" ((,) <$> requireAttr "chapter" <*> optionalAttr "section") parseLink
    , tag' "abbr" (requireAttr "title") parseAbbr
    ]

parseXref :: MonadIO m => Text -> Iteratee SEvent m Inline
parseXref href = do
    inner <- content'
    return $ Xref href inner

parseLink :: MonadIO m => (Text, Maybe Text) -> Iteratee SEvent m Inline
parseLink (chapter, section) = do
    inner <- content'
    return $ Link chapter section inner

parseAbbr :: MonadIO m => Text -> Iteratee SEvent m Inline
parseAbbr title = do
    inner <- content'
    return $ Abbr title inner

parseListItem :: MonadIO m => Iteratee SEvent m (Maybe ListItem)
parseListItem = tag'' "li" $ ListItem <$> many parseInline

instance IsString Name where
    fromString s = Name (T.pack s) Nothing Nothing
