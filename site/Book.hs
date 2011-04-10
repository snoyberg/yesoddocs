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

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.XML.Enumerator.Parse
import Data.XML.Types (Event)
import Data.Enumerator (Iteratee, throwError)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Text.Blaze (ToHtml (..))
import qualified Text.Highlighting.Kate as Kate
import qualified System.IO.UTF8 as U
import Data.List (group, sort)
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
instance ToHtml ChapterStatus where toHtml = toHtml . show

data Chapter = Chapter
    { chapterSlug :: Text
    , chapterTitle :: Text
    , chapterStatus :: ChapterStatus
    , chapterIntro :: [Block]
    , chapterSynopsis :: Maybe [Kate.SourceLine]
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
           | Example Text
    deriving Show

data Inline = Inline Text
            | Emphasis [Inline]
            | Strong [Inline]
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
loadBook = do
    --events <- parseFile_ "book/book.xml" decodeEntities consume
    parseFile_ "book/book.xml" decodeEntities parseBook

parseBook :: MonadIO m => Iteratee Event m Book
parseBook = force "Missing book" $ tagNoAttr "book" $ do
    title <- force "Book requires title" titleTag
    parts <- many $ tagNoAttr "part" parsePart
    return $ Book title parts

titleTag :: Monad m => Iteratee Event m (Maybe Text)
titleTag = tagNoAttr "title" content

parsePart :: MonadIO m => Iteratee Event m Part
parsePart = do
    title <- force "Part requires title" titleTag
    chapters <- many $ tagName "chapter" (requireAttr "href") parseChapter'
    return $ Part title chapters

parseChapter' :: MonadIO m => Text -> Iteratee Event m Chapter
parseChapter' href' = do
    x <- liftIO $ parseFile ("book/" ++ T.unpack href') decodeEntities parseChapter
    case x of
        Left e -> throwError e
        Right c -> return c

parseChapter :: MonadIO m => Iteratee Event m Chapter
parseChapter = force "Missing chapter" $ tagName "chapter" ((,) <$> requireAttr "id" <*> requireAttr "status") $ \(slug, status') -> do
    status <- readStatus status'
    title <- force "Chapter requires title" titleTag
    intro <- force "Intro required" $ tagNoAttr "intro" $ many parseBlock
    msynopsis <- tagName "synopsis" (requireAttr "name") $ \name -> do
        raw <- liftIO $ U.readFile $ "snippets/" ++ T.unpack name ++ ".hs" -- FIXME use text
        case Kate.highlightAs "haskell" raw of
            Left e -> throwError $ XmlException (concat
                [ "Could not parse synopsis "
                , T.unpack name
                , ": "
                , e
                ]) Nothing
            Right lines' -> return lines'
    sections <- many $ tagName "section" (requireAttr "id") parseSection
    checkUniqueIds slug sections
    summary <- tagNoAttr "summary" $ many parseBlock
    return $ Chapter slug title status intro msynopsis sections summary
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

parseSection :: MonadIO m => Text -> Iteratee Event m Section
parseSection id' = do
    title <- force "Section requires title" titleTag
    blocks <- many parseBlockSection
    return $ Section id' title blocks

parseBlockSection :: MonadIO m => Iteratee Event m (Maybe (Either Section Block))
parseBlockSection = choose
    [ (fmap . fmap) Left $ tagName "section" (requireAttr "id") $ parseSection
    , (fmap . fmap) Right parseBlock
    ]

parseBlock :: MonadIO m => Iteratee Event m (Maybe Block)
parseBlock = choose
    [ tagName "p" (requireAttr "id") $ \id' -> do
        ins <- many parseInline
        return $ Paragraph id' ins
    , tagNoAttr "ul" $ fmap UList $ many parseListItem
    , tagNoAttr "ol" $ fmap OList $ many parseListItem
    , tagNoAttr "codeblock" $ fmap CodeBlock content
    , tagName "snippet" (requireAttr "name") parseSnippet
    , tagNoAttr "advanced" $ fmap Advanced $ many parseBlock
    , tagNoAttr "note" $ fmap Note $ many parseInline
    , tagName "image" (Image <$> requireAttr "src" <*> requireAttr "title") return
    , tagNoAttr "defs" $ fmap Defs $ many $ tagName "def" (requireAttr "term") parseDef
    , tagNoAttr "markdown" $ fmap Markdown content -- FIXME this must be killed with fire
    , tagName "example" (requireAttr "name") parseExample
    ]

parseExample :: MonadIO m => Text -> Iteratee Event m Block
parseExample name =
    liftIO $ fmap Example $ TIO.readFile fp
  where
    fp = "yesod-examples/src/" ++ T.unpack name ++ ".lhs"

parseDef :: MonadIO m => Text -> Iteratee Event m (Text, [Inline])
parseDef term = do
    c <- many parseInline
    return (term, c)

parseSnippet :: MonadIO m => Text -> Iteratee Event m Block
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

parseInline :: MonadIO m => Iteratee Event m (Maybe Inline)
parseInline = choose
    [ (fmap . fmap) Inline contentMaybe
    , tagNoAttr "i" $ fmap Emphasis $ many parseInline
    , tagNoAttr "b" $ fmap Strong $ many parseInline
    , tagNoAttr "term" $ fmap Term content
    , tagNoAttr "hackage" $ fmap Hackage content
    , tagName "xref" (requireAttr "href") parseXref
    , tagNoAttr "code" $ fmap Code content
    , tagName "link" ((,) <$> requireAttr "chapter" <*> optionalAttr "section") parseLink
    , tagName "abbr" (requireAttr "title") parseAbbr
    ]

parseXref :: MonadIO m => Text -> Iteratee Event m Inline
parseXref href = do
    inner <- content
    return $ Xref href inner

parseLink :: MonadIO m => (Text, Maybe Text) -> Iteratee Event m Inline
parseLink (chapter, section) = do
    inner <- content
    return $ Link chapter section inner

parseAbbr :: MonadIO m => Text -> Iteratee Event m Inline
parseAbbr title = do
    inner <- content
    return $ Abbr title inner

parseListItem :: MonadIO m => Iteratee Event m (Maybe ListItem)
parseListItem = tagNoAttr "li" $ ListItem <$> many parseInline
