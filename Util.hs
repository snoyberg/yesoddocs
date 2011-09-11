{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Util
    ( renderContent
    , validateContent
    , validateDita
    , userGravatar
    , prettyDate
    , prettyMonthYear
    , formatDateTime
    , prettyDateTime
    ) where

import Debug.Trace (trace)
import Model (TopicFormat (..), User, userEmail, TopicId)
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (encodeUtf8)
import Text.Blaze (Html, preEscapedText, toHtml, preEscapedString)
import Text.HTML.SanitizeXSS (sanitizeBalance)
import Text.Hamlet (shamlet)
import Data.Digest.Pure.MD5 (md5)
import Data.Char (isSpace, toLower)
import Data.Maybe (fromMaybe, catMaybes)
import qualified Data.ByteString.Lazy as L
import Text.Pandoc (writeHtmlString, defaultWriterOptions, readMarkdown, defaultParserState)
import qualified Data.Text as T
import Yesod.Form (Textarea (Textarea))
import System.Locale
import Data.Time (formatTime, UTCTime, fromGregorian)
import Text.XML.Enumerator.Parse (parseText, decodeEntities)
import Text.XML.Enumerator.Document (fromEvents)
import Data.Enumerator (run, ($$), joinI, enumList, run_)
import Data.Enumerator.List (consume)
import Data.Functor.Identity (runIdentity)
import Data.XML.Types (Node (..), Content (..), Document (..), Element (..), Event (..), nameLocalName)
import Wiki (WikiRoute (..), fromSinglePiece)
import Data.Maybe (fromJust)
import Text.Hamlet (HtmlUrl)
import qualified Data.Set as Set
import Control.Monad.Trans.State (evalState, get, put)
import Text.XML.Enumerator.Render (renderText)
import Data.Monoid (mconcat, mempty, mappend)
import System.IO.Unsafe (unsafePerformIO)
import Yesod.Core (toSinglePiece)
import qualified Data.Map as Map
import qualified Text.Highlighting.Illuminate as I
import qualified Text.Highlighting.Illuminate.Haskell as Haskell
import qualified Text.XHtml
import Data.Char (isUpper)

renderContent :: TopicId -> TopicFormat -> Text -> HtmlUrl WikiRoute
renderContent _ TFHtml t = const $ preEscapedText t
renderContent _ TFText t = const $ toHtml $ Textarea t
renderContent _ TFMarkdown t = const $ preEscapedString $ writeHtmlString defaultWriterOptions $ readMarkdown defaultParserState $ unpack t
renderContent tid TFDitaConcept t = ditaToHtml tid t
renderContent tid TFDitaTopic t = ditaToHtml tid t

ditaToHtml :: TopicId -> Text -> HtmlUrl WikiRoute
ditaToHtml topic txml render =
    case runIdentity $ run $ enumList 3 ["<body>", txml, "</body>"] $$ joinI $ parseText decodeEntities $$ fromEvents of
        Left e -> toHtml $ show e
        Right (Document _ (Element _ _ nodes) _) -> mapM_ go nodes
  where
    go (NodeContent (ContentText t')) = toHtml t'
    go (NodeElement (Element n as children)) = go' (nameLocalName n) as children $ mapM_ go children
    go _ = return ()
    go' "p" as _ x =
        case lookup "id" as of
            Just [ContentText t] -> [shamlet|<p .hascomments #comment-#{toSinglePiece topic}-#{t}>#{x}|]
            _ -> [shamlet|<p>#{x}|]
    go' "image" as _ x =
        case lookup "href" as of
            Just [ContentText t] -> [shamlet|<img src=#{toLink t}>|]
            _ -> x
    go' "xref" as _ x =
        case lookup "href" as of
            Just [ContentText t] -> [shamlet|<a href=#{toLink t}>#{x}|]
            _ -> x
    go' "apiname" _ [NodeContent (ContentText x)] _ =
        let (href, display) = renderApiname x
         in [shamlet|<a href=#{href}>#{display}|]
    go' "codeblock" as [NodeContent (ContentText t)] _
        | lookup "outputclass" as == Just [ContentText "lhaskell"] = lhaskellToHTML t
        | lookup "outputclass" as == Just [ContentText "haskell"] = haskellToHTML 1 t
    go' "codeblock" _ _ x = [shamlet|<pre>
    <code>#{x}|]
    go' "note" as _ x =
        case lookup "type" as of
            Just [ContentText nt]
                | nt == "other" ->
                    case lookup "othertype" as of
                        Just [ContentText ot] -> [shamlet|<aside .note-#{ot}>#{x}|]
                        _ -> [shamlet|<aside .note-#{x}>|]
                | otherwise -> [shamlet|<aside .#{nt}>#{x}|]
            _ -> [shamlet|<aside .note>#{x}|]
    go' n _ _ x
        | n `Set.member` ditaDrop = [shamlet||]
        | n `Set.member` ditaIgnore = x
        | n `Set.member` ditaUnchanged = [shamlet|\<#{n}>#{x}</#{n}>|]
    go' n _ _ x =
        case Map.lookup n ditaMap of
            Just (n', Nothing) ->
                [shamlet|\<#{n'}>#{x}</#{n'}>|]
            Just (n', Just c) ->
                [shamlet|\<#{n'} .#{c}>#{x}</#{n'}>|]
            Nothing ->
                trace ("Unknown DITA element: " ++ show n) $
                [shamlet|<h1 style=color:red>Unknown DITA element: #{show n}|]
    toLink t
        | topicPref `T.isPrefixOf` t =
            let suffix = T.drop (T.length topicPref) t
                (tid, rest) = T.break (== '#') suffix
             in render (BestTopicR $ fromJust $ fromSinglePiece tid) [] `T.append` rest
        | staticPref `T.isPrefixOf` t = render (StaticContentR $ fromJust $ fromSinglePiece $ T.drop (T.length staticPref) t) []
        | "yw://" `T.isPrefixOf` t = "FIXME: " `T.append` t
        | otherwise = t
    topicPref = "yw://topic/"
    staticPref = "yw://static/"

validateContent :: TopicFormat -> Text -> Text
validateContent TFHtml t = sanitizeBalance t
validateContent TFText t = t
validateContent TFMarkdown t = T.filter (/= '\r') t
validateContent TFDitaConcept t = validateDita t
validateContent TFDitaTopic t = validateDita t

validateDita :: Text -> Text
validateDita txml =
    case runIdentity $ run $ enumList 3 ["<body>", txml, "</body>"] $$ joinI $ parseText decodeEntities $$ fromEvents of
        Left e -> pack $ show e
        Right (Document _ (Element _ _ nodes) _) -> nodesToText $ flip evalState (Set.empty, 1 :: Int) $ do
            nodes' <- mapM checkUnused nodes
            mapM addIds nodes'
  where
    checkUnused (NodeElement (Element e as ns)) = do
        as' <- fmap catMaybes $ mapM checkUnused' as
        ns' <- mapM checkUnused ns
        return $ NodeElement $ Element e as' ns'
    checkUnused n = return n
    checkUnused' ("id", [ContentText t]) = do
        (s, i) <- get
        if t `Set.member` s
            then return Nothing
            else do
                put (Set.insert t s, i)
                return $ Just ("id", [ContentText t])
    checkUnused' ("id", _) = return Nothing
    checkUnused' a = return $ Just a
    addIds (NodeElement (Element e as ns)) = do
        as' <-
            case lookup "id" as of
                Just _ -> return as
                Nothing -> do
                    i <- getId
                    return $ ("id", [ContentText i]) : as
        ns' <- mapM addIds ns
        return $ NodeElement $ Element e as' ns'
    addIds n = return n
    getId = do
        (s, i0) <- get
        let go i =
                let t = pack $ 'x' : show i
                 in if t `Set.member` s
                        then go $ i + 1
                        else (t, i + 1)
        let (t, i') = go i0
        put (Set.insert t s, i')
        return t

nodesToText :: [Node] -> Text
nodesToText input =
    mconcat $ unsafePerformIO $ run_ $ enumList 8 (goN input []) $$ joinI $ renderText $$ consume
  where
    goN :: [Node] -> [Event] -> [Event]
    goN [] e = e
    goN (NodeContent c:ns) e = EventContent c : goN ns e
    goN (NodeInstruction i:ns) e = EventInstruction i : goN ns e
    goN (NodeComment c:ns) e = EventComment c : goN ns e
    goN (NodeElement el:ns) e = goE el $ goN ns e

    goE :: Element -> [Event] -> [Event]
    goE (Element n as ns) evs =
        EventBeginElement n as
      : goN ns
      ( EventEndElement n
      : evs)

userGravatar :: User -> Html
userGravatar u =
    [shamlet|<img src="http://www.gravatar.com/avatar/#{hash}?d=identicon&s=100">|]
  where
    email = fromMaybe "" $ userEmail u
    hash = pack $ show $ md5 $ L.fromChunks $ return $ encodeUtf8 $ pack $ map toLower $ trim $ unpack email
    trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

prettyDate :: UTCTime -> String
prettyDate = formatTime defaultTimeLocale "%B %e, %Y" -- FIXME i18n

prettyMonthYear :: Int -> Int -> String
prettyMonthYear year month = formatTime defaultTimeLocale "%B %Y" $ fromGregorian (fromIntegral year) month 1 -- FIXME i18n

formatDateTime :: UTCTime -> String
formatDateTime = formatTime defaultTimeLocale "%FT%T%z"

prettyDateTime :: UTCTime -> String
prettyDateTime = formatTime defaultTimeLocale "%B %e, %y %l:%M %P"

-- drop element and children
ditaDrop :: Set.Set Text
ditaDrop = Set.fromList $ T.words "draft-comment indexterm colspec"

-- ignore this element, but continue with children
ditaIgnore :: Set.Set Text
ditaIgnore = Set.fromList $ T.words "dlhead dlentry ph keyword menucascade plentry tgroup"

-- translate as-is
ditaUnchanged :: Set.Set Text
ditaUnchanged = Set.fromList $ T.words "table ul ol li i b dl dt dd sup cite q thead tbody pre section"

ditaMap :: Map.Map Text (Text, Maybe Text)
ditaMap = Map.fromList
    [ ("userinput", elem' "kbd")
    , ("lq", elem' "blockquote")
    , ("row", elem' "tr")
    , ("sthead", elem' "thead")
    , ("parml", elem' "dl")
    , ("pt", elem' "dt")
    , ("pd", elem' "dd")
    , ("fig", elem' "figure")
    , ("title", elem' "h1")
    , ("codeph", elem' "code")
    , ("varname", elem' "code")
    , ("msgph", elem' "code")
    , ("option", elem' "i")
    , ("lines", div' "lines")
    , ("desc", div' "desc") -- FIXME
    , ("term", elem' "b")
    , ("uicontrol", elem' "code")
    , ("dthd", ("dt", Just "head"))
    , ("ddhd", ("dd", Just "head"))
    , ("shortcut", elem' "i")
    , ("figgroup", elem' "section")
    , ("sl", elem' "ul")
    , ("sli", elem' "li")
    , ("filepath", elem' "code")
    , ("simpletable", elem' "table")
    , ("strow", elem' "tr")
    , ("stentry", elem' "td")
    , ("entry", elem' "td")
    , ("example", elem' "section")
    ]
  where
    elem' x = (x, Nothing)
    div' x = ("div", Just x)

lhaskellToHTML :: Text -> Html
lhaskellToHTML =
    go' 1 . map go . T.lines
  where
    go t
        | a == "> " = Right b
        | a' == ">" = Right b'
        | otherwise = Left $ [shamlet|<p>#{preEscapedText t}|]
      where
        (a, b) = T.splitAt 2 t
        (a', b') = T.splitAt 1 t
    go' _ [] = mempty
    go' i (Left x:xs) = x `mappend` go' i xs
    go' i x =
        y' `mappend` go' (i + length y) z
      where
        (y, z) = someRights id x
        y' = haskellToHTML i (T.unlines y)
    someRights front (Right x:xs) = someRights (front . (:) x) xs
    someRights front x = (front [], x)

haskellToHTML :: Int -> Text -> Html
haskellToHTML line t =
    case I.tokenize (Just Haskell.lexer) str' of
        Left _ -> [shamlet|<code>
    <pre>#{t}|]
        Right ts ->
            let h = preEscapedString $ Text.XHtml.showHtmlFragment $ I.toXHtmlInline (I.defaultOptions
                        { I.optNumberLines = True
                        , I.optStartNumber = line
                        }) ts
             in [shamlet|<div .haskell>#{h}|]
  where
    str = noCR . T.unpack t
    noCR = filter (/='\r')
    lines' = lines str
    hasStart = any (== "-- START") lines'
    str' = if hasStart
            then unlines $ go False lines'
            else str
    go _ [] = []
    go _ ("-- START":rest) = go True rest
    go _ ("-- STOP":rest) = go False rest
    go True (x:xs) = x : go True xs
    go False (_:xs) = go False xs

renderApiname :: Text -> (Text, Text)
renderApiname a
    | T.null modul = ("http://hackage.haskell.org/package/" `T.append` a, a)
    | T.null ident = (modulHref, modul)
    | otherwise = (identHref, ident)
  where
    (package, b) = T.break (== ':') a
    (modul, c) = T.break (== ':') $ T.drop 1 b
    ident = T.drop 1 c

    modulHref = T.concat
        [ "http://hackage.haskell.org/packages/archive/"
        , package
        , "/latest/doc/html/"
        , T.map dotToDash modul
        , ".html"
        ]
    dotToDash '.' = '-'
    dotToDash c' = c'

    identHref = T.concat
        [ modulHref
        , "#"
        , if isUpper (T.head ident) then "t" else "v"
        , ":"
        , ident
        ]
