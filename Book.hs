{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
module Book
    where

import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text as TS
import Text.XML.LibXML.Enumerator
import Data.XML.Types
import qualified Data.Enumerator as E
import Data.Enumerator (Iteratee, throwError, ($$), joinI, run)
import Data.Enumerator.IO
import Data.Typeable (Typeable)
import Control.Exception (Exception, SomeException, throwIO)
import Data.Char (isSpace)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Text.Hamlet (ToHtml (..), string)

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
    { sectionId :: Maybe Text
    , sectionTitle :: Text
    , sectionBlocks :: [Either Section Block] -- FIXME allow subsections
    }
    deriving Show

data Block = Paragraph [Inline]
           | UList [ListItem]
           | CodeBlock Text
           | Snippet Text
           | Advanced [Block]
           | Note [Inline]
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

data ListItem = ListItem [Inline] -- FIXME block or inline
    deriving Show

main :: IO ()
main = parseFile "book.xml" parseBook >>= print

loadBook :: IO Book
loadBook = do
    x <- parseFile "book.xml" parseBook
    case x of
        Left e -> throwIO e
        Right y -> return y

parseFile :: String -> Iteratee Event IO a -> IO (Either SomeException a)
parseFile fn p =
    run $ enumFile fn $$ joinI $ parseBytesIO (Just $ TS.pack fn)
        $$ between EventBeginDocument EventEndDocument p

data XmlException = XmlException String
    deriving (Show, Typeable)
instance Exception XmlException

expect :: MonadIO m => String -> (Event -> Maybe b) -> Iteratee Event m b
expect msg f = do
    y <- E.head
    case y of
        Nothing -> throwError $ XmlException "Unexpected end-of-stream"
        Just z ->
            case f z of
                Nothing -> throwError $ XmlException $ "Unexpected: " ++ show z ++ ", " ++ msg
                Just x -> return x

require :: MonadIO m => Event -> Iteratee Event m ()
require x =
    expect (" expected " ++ show x) go
  where
    go y
        | x == y = Just ()
        | otherwise = Nothing

between :: MonadIO m => Event -> Event -> Iteratee Event m a -> Iteratee Event m a
between start end middle = do
    require start
    x <- middle
    require end
    return x

whitespace :: MonadIO m => Iteratee Event m ()
whitespace = do
    x <- E.peek
    isWhite <-
        case x of
            Just EventComment{} -> return True
            Just (EventContent c) -> T.all isSpace `fmap` toText c
            _ -> return False
    if isWhite
        then E.drop 1 >> whitespace
        else return ()

tagsAttr :: MonadIO m
         => Text
         -> ([Attribute] -> Iteratee Event m a)
         -> Iteratee Event m [a]
tagsAttr t i =
    go id
  where
    go front = do
        whitespace
        x <- E.peek
        let name = Name t Nothing Nothing
        case x of
            Just (EventBeginElement name' attrs)
                | name == name' -> do
                    E.drop 1
                    y <- i attrs
                    whitespace
                    require $ EventEndElement name
                    go $ front . (:) y
            _ -> return $ front []

tags :: MonadIO m => Text -> Iteratee Event m a -> Iteratee Event m [a]
tags t i =
    go id
  where
    go front = do
        whitespace
        x <- E.peek
        let name = Name t Nothing Nothing
        if x == Just (EventBeginElement name [])
            then do
                E.drop 1
                y <- i
                whitespace
                require $ EventEndElement name
                go $ front . (:) y
            else return $ front []

tagAttr :: MonadIO m
        => Text
        -> ([Attribute] -> Iteratee Event m a)
        -> Iteratee Event m a
tagAttr t i = do
    whitespace
    attrs <- expect (show t ++ " tag with attributes") $ \e ->
                case e of
                    EventBeginElement name' attrs
                        | name == name' -> Just attrs
                    _ -> Nothing
    x <- i attrs
    whitespace
    require end
    return x
  where
    name = Name t Nothing Nothing
    end = EventEndElement name

tag :: MonadIO m => Text -> Iteratee Event m a -> Iteratee Event m a
tag t i = do
    whitespace
    require begin
    x <- i
    whitespace
    require end
    return x
  where
    name = Name t Nothing Nothing
    begin = EventBeginElement name []
    end = EventEndElement name

mtagAttrs :: MonadIO m
          => Text
          -> ([Attribute] -> Iteratee Event m a)
          -> Iteratee Event m (Maybe a)
mtagAttrs t i = do
    whitespace
    x <- E.peek
    case x of
        Just (EventBeginElement name' attrs)
            | name == name' -> do
                E.drop 1
                y <- i attrs
                whitespace
                require end
                return $ Just y
        _ -> return Nothing
  where
    name = Name t Nothing Nothing
    end = EventEndElement name

mtag :: MonadIO m => Text -> Iteratee Event m a -> Iteratee Event m (Maybe a)
mtag t i = do
    whitespace
    x <- E.peek
    if x == Just begin
        then do
            E.drop 1
            y <- i
            whitespace
            require end
            return $ Just y
        else return Nothing
  where
    name = Name t Nothing Nothing
    begin = EventBeginElement name []
    end = EventEndElement name

textTag :: MonadIO m => Text -> Iteratee Event m Text
textTag = flip tag takeText

takeText :: MonadIO m => Iteratee Event m Text
takeText =
    go id
  where
    go front = do
        x <- E.peek
        case x of
            Just EventComment{} -> E.drop 1 >> go front
            Just (EventContent c) -> E.drop 1 >> go (front . (:) c)
            _ -> fmap T.concat $ mapM toText $ front []

toText :: MonadIO m => Content -> Iteratee Event m Text
toText (ContentText t) = return t
toText (ContentEntity e)
    | e == "lt" = return "<"
    | e == "gt" = return ">"
    | e == "amp" = return "&"
    | e == "quot" = return "\""
    | e == "apos" = return "'"
    | otherwise = error $ "toText: " ++ show e

parseBook :: MonadIO m => Iteratee Event m Book
parseBook = tag "book" $ do
        title <- textTag "title"
        parts <- tags "part" parsePart
        return $ Book title parts

parsePart :: MonadIO m => Iteratee Event m Part
parsePart = do
    title <- textTag "title"
    chapters <- tagsAttr "chapter" parseChapter'
    return $ Part title chapters

parseChapter' :: MonadIO m => [Attribute] -> Iteratee Event m Chapter
parseChapter' [Attribute name href]
    | name == Name "href" Nothing Nothing = do
        href' <- fmap T.concat $ mapM toText href
        x <- liftIO $ parseFile (T.unpack href') parseChapter
        case x of
            Left e -> throwError e
            Right c -> return c
parseChapter' _ = throwError $ XmlException "Expected one href attr for chapter"

parseChapter :: MonadIO m => Iteratee Event m Chapter
parseChapter = tagAttr "chapter" $ \attrs -> do
    slug <- getAttribute "id" attrs
    status <- getAttribute "status" attrs >>= readStatus
    title <- textTag "title"
    intro <- tag "intro" $ many parseBlock
    sections <- tagsAttr "section" parseSection
    summary <- mtag "summary" $ many parseBlock
    return $ Chapter slug title status intro sections summary
  where
    readStatus t =
        case reads $ T.unpack t of
            [] -> throwError $ XmlException $ "Invalid status: " ++ show t
            (s, _):_ -> return s

mgetAttribute :: MonadIO m => Text -> [Attribute] -> Iteratee Event m (Maybe Text)
mgetAttribute t as =
    case lookup t' $ map (\(Attribute x y) -> (x, y)) as of
        Nothing -> return Nothing
        Just v -> fmap (Just . T.concat) $ mapM toText v
  where
    t' = Name t Nothing Nothing

getAttribute :: MonadIO m => Text -> [Attribute] -> Iteratee Event m Text
getAttribute t as =
    case lookup t' $ map (\(Attribute x y) -> (x, y)) as of
        Nothing -> throwError $ XmlException $ "Missing attribute: " ++ show t
        Just v -> fmap T.concat $ mapM toText v
  where
    t' = Name t Nothing Nothing

parseSection :: MonadIO m => [Attribute] -> Iteratee Event m Section
parseSection attrs = do
    id' <- mgetAttribute "id" attrs
    title <- textTag "title"
    blocks <- many parseBlockSection
    return $ Section id' title blocks

many :: Monad m => Iteratee Event m (Maybe a) -> Iteratee Event m [a]
many i =
    go id
  where
    go front = do
        x <- i
        case x of
            Nothing -> return $ front []
            Just y -> go $ front . (:) y

parseBlockSection :: MonadIO m => Iteratee Event m (Maybe (Either Section Block))
parseBlockSection = choose
    [ (fmap . fmap) Left $ mtagAttrs "section" $ parseSection
    , (fmap . fmap) Right parseBlock
    ]

parseBlock :: MonadIO m => Iteratee Event m (Maybe Block)
parseBlock = choose
    [ mtag "p" $ fmap Paragraph $ many parseInline
    , mtag "ul" $ fmap UList $ many parseListItem
    , mtag "codeblock" $ fmap CodeBlock takeText
    , mtagAttrs "snippet" parseSnippet
    , mtag "advanced" $ fmap Advanced $ many parseBlock
    , mtag "note" $ fmap Note $ many parseInline
    ]

parseSnippet :: MonadIO m => [Attribute] -> Iteratee Event m Block
parseSnippet attrs = do
    name <- getAttribute "name" attrs
    return $ Snippet $ T.concat
        [ "Normally I would include snippet "
        , name
        , " here. FIXME!"
        ]

parseInline :: MonadIO m => Iteratee Event m (Maybe Inline)
parseInline = choose
    [ (fmap . fmap) Inline parseText
    , mtag "i" $ fmap Emphasis $ many parseInline
    , mtag "term" $ fmap Term takeText
    , mtag "hackage" $ fmap Hackage takeText
    , mtagAttrs "xref" parseXref
    , mtag "code" $ fmap Code takeText
    , mtagAttrs "link" parseLink
    , mtagAttrs "abbr" parseAbbr
    ]

parseXref :: MonadIO m => [Attribute] -> Iteratee Event m Inline
parseXref attrs = do
    href <- getAttribute "href" attrs
    inner <- takeText
    return $ Xref href inner

parseLink :: MonadIO m => [Attribute] -> Iteratee Event m Inline
parseLink attrs = do
    chapter <- getAttribute "chapter" attrs
    section <- mgetAttribute "section" attrs
    inner <- takeText
    return $ Link chapter section inner

parseAbbr :: MonadIO m => [Attribute] -> Iteratee Event m Inline
parseAbbr attrs = do
    title <- getAttribute "title" attrs
    inner <- takeText
    return $ Abbr title inner

parseListItem :: MonadIO m => Iteratee Event m (Maybe ListItem)
parseListItem = mtag "li" $ fmap ListItem $ many parseInline

parseText :: MonadIO m => Iteratee Event m (Maybe Text)
parseText = do
    x <- E.peek
    case x of
        Just EventComment{} -> E.drop 1 >> parseText
        Just (EventContent c) -> E.drop 1 >> fmap Just (toText c)
        _ -> return Nothing

choose :: MonadIO m => [Iteratee Event m (Maybe a)]
       -> Iteratee Event m (Maybe a)
choose [] = return Nothing
choose (i:is) = do
    x <- i
    case x of
        Nothing -> choose is
        Just y -> return $ Just y
