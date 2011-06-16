{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Util
    ( renderContent
    , validateContent
    , validateDita
    , userGravatar
    , prettyDate
    , prettyMonthYear
    ) where

import Debug.Trace (trace)
import Model (TopicFormat (..), User (userEmail), TopicId)
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (encodeUtf8)
import Text.Hamlet (Html, preEscapedText, toHtml, preEscapedString)
import Text.HTML.SanitizeXSS (sanitizeBalance)
import Text.Hamlet.NonPoly (html)
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
import Data.XML.Types (Node (..), Content (..), Document (..), Element (..), Event (..))
import Wiki (WikiRoute (..), fromSinglePiece)
import Data.Maybe (fromJust)
import Text.Hamlet (Hamlet)
import qualified Data.Set as Set
import Control.Monad.Trans.State (evalState, get, put)
import Text.XML.Enumerator.Render (renderText)
import Data.Monoid (mconcat)
import System.IO.Unsafe (unsafePerformIO)
import Yesod.Core (toSinglePiece)

renderContent :: TopicId -> TopicFormat -> Text -> Hamlet WikiRoute
renderContent _ TFHtml t = const $ preEscapedText t
renderContent _ TFText t = const $ toHtml $ Textarea t
renderContent _ TFMarkdown t = const $ preEscapedString $ writeHtmlString defaultWriterOptions $ readMarkdown defaultParserState $ unpack t
renderContent tid TFDitaConcept t = ditaToHtml tid t
renderContent tid TFDitaTopic t = ditaToHtml tid t

ditaToHtml :: TopicId -> Text -> Hamlet WikiRoute
ditaToHtml topic txml render =
    case runIdentity $ run $ enumList 3 ["<body>", txml, "</body>"] $$ joinI $ parseText decodeEntities $$ fromEvents of
        Left e -> toHtml $ show e
        Right (Document _ (Element _ _ nodes) _) -> mapM_ go nodes
  where
    go (NodeContent (ContentText t')) = toHtml t'
    go (NodeElement (Element n as children)) = go' n as $ mapM_ go children
    go _ = return ()
    go' "p" as x =
        case lookup "id" as of
            Just [ContentText t] -> [html|<p .hascomments #comment-#{toSinglePiece topic}-#{t}>#{x}|]
            _ -> [html|<p>#{x}|]
    go' "ul" _ x = [html|<ul>#{x}|]
    go' "ol" _ x = [html|<ol>#{x}|]
    go' "li" _ x = [html|<li>#{x}|]
    go' "i" _ x = [html|<i>#{x}|]
    go' "b" _ x = [html|<b>#{x}|]
    go' "fig" _ x = [html|<figure>#{x}|]
    go' "title" _ x = [html|<h1>#{x}|]
    go' "image" as x =
        case lookup "href" as of
            Just [ContentText t] -> [html|<img src=#{toLink t}>|]
            _ -> x
    go' "xref" as x =
        case lookup "href" as of
            Just [ContentText t] -> [html|<a href=#{toLink t}>#{x}|]
            _ -> x
    go' "codeph" _ x = [html|<code>#{x}|]
    go' "draft-comment" _ _ = [html||]
    go' "indexterm" _ _ = [html||]
    go' "lines" _ x = [html|<.lines>#{x}|]
    go' "term" _ x = [html|<b>#{x}|]
    go' "dl" _ x = [html|<dl>#{x}|]
    go' "dlhead" _ x = [html|#{x}|]
    go' "sup" _ x = [html|<sup>#{x}|]
    go' "dlentry" _ x = [html|#{x}|]
    go' "dt" _ x = [html|<dt>#{x}|]
    go' "dd" _ x = [html|<dd>#{x}|]
    go' "dthd" _ x = [html|<dt .head>#{x}|]
    go' "ddhd" _ x = [html|<dd .head>#{x}|]
    go' "shortcut" _ x = [html|<i>#{x}|]
    go' "figgroup" _ x = [html|<section>#{x}|]
    go' "desc" _ x = [html|<div .desc>#{x}|] -- FIXME
    go' "ph" _ x = [html|#{x}|]
    go' "varname" _ x = [html|<code>#{x}|]
    go' "option" _ x = [html|<i>#{x}|]
    go' "msgph" _ x = [html|<code>#{x}|]
    go' "keyword" _ x = [html|#{x}|]
    go' "cite" _ x = [html|<cite>#{x}|]
    go' "sl" _ x = [html|<ul>#{x}|]
    go' "sli" _ x = [html|<li>#{x}|]
    go' "table" _ x = [html|<table>#{x}|]
    go' "filepath" _ x = [html|<code>#{x}|]
    go' "simpletable" _ x = [html|<table>#{x}|]
    go' "strow" _ x = [html|<tr>#{x}|]
    go' "stentry" _ x = [html|<td>#{x}|]
    go' "entry" _ x = [html|<td>#{x}|]
    go' "userinput" _ x = [html|<kbd>#{x}|]
    go' "lq" _ x = [html|<blockquote>#{x}|]
    go' "menucascade" _ x = [html|#{x}|]
    go' "parml" _ x = [html|<dl>#{x}|]
    go' "plentry" _ x = [html|#{x}|]
    go' "pt" _ x = [html|<dt>#{x}|]
    go' "pd" _ x = [html|<dd>#{x}|]
    go' "q" _ x = [html|<q>#{x}|]
    go' "uicontrol" _ x = [html|<code>#{x}|]
    go' "colspec" _ x = [html|#{x}|]
    go' "sthead" _ x = [html|<thead>#{x}|]
    go' "thead" _ x = [html|<thead>#{x}|]
    go' "tbody" _ x = [html|<tbody>#{x}|]
    go' "row" _ x = [html|<tr>#{x}|]
    go' "tgroup" _ x = [html|#{x}|]
    go' "pre" _ x = [html|<pre>#{x}|]
    go' "example" _ x = [html|<section>
<h1>Example
\#{x}|]
    go' "section" _ x = [html|<section>#{x}|]
    go' "apiname" _ x = [html|<a href="http://hackage.haskell.org/package/#{x}">#{x}|]
    go' "codeblock" _ x = [html|<pre>
    <code>#{x}|]
    go' "note" as x =
        case lookup "type" as of
            Just [ContentText nt]
                | nt == "other" ->
                    case lookup "othertype" as of
                        Just [ContentText ot] -> [html|<.note-#{ot}>#{x}|]
                        _ -> [html|<.note#{x}>|]
                | otherwise -> [html|<.#{nt}>#{x}|]
            _ -> [html|<.note>#{x}|]
    go' n _ _ =
        trace ("Unknown DITA element: " ++ show n) $
        [html|<h1 style=color:red>Unknown DITA element: #{show n}|]
    toLink t
        | topicPref `T.isPrefixOf` t =
            let suffix = T.drop (T.length topicPref) t
                (tid, rest) = T.break (== '#') suffix
             in render (TopicR $ fromJust $ fromSinglePiece tid) [] `T.append` rest
        | staticPref `T.isPrefixOf` t = render (StaticContentR $ fromJust $ fromSinglePiece $ T.drop (T.length staticPref) t) []
        | "yw://" `T.isPrefixOf` t = "FIXME: " `T.append` t
        | otherwise = t
    topicPref = "yw://topic/"
    staticPref = "yw://static/"

validateContent :: TopicFormat -> Text -> Text
validateContent TFHtml t = pack $ sanitizeBalance $ unpack t
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
    [html|<img src="http://www.gravatar.com/avatar/#{hash}?d=identicon&s=100">|]
  where
    email = fromMaybe "" $ userEmail u
    hash = pack $ show $ md5 $ L.fromChunks $ return $ encodeUtf8 $ pack $ map toLower $ trim $ unpack email
    trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

prettyDate :: UTCTime -> String
prettyDate = formatTime defaultTimeLocale "%B %e, %Y" -- FIXME i18n

prettyMonthYear :: Int -> Int -> String
prettyMonthYear year month = formatTime defaultTimeLocale "%B %Y" $ fromGregorian (fromIntegral year) month 1 -- FIXME i18n
