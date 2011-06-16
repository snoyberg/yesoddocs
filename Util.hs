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
import Data.XML.Types (Node (..), Content (..), Document (..), Element (..), Event (..), nameLocalName)
import Wiki (WikiRoute (..), fromSinglePiece)
import Data.Maybe (fromJust)
import Text.Hamlet (Hamlet)
import qualified Data.Set as Set
import Control.Monad.Trans.State (evalState, get, put)
import Text.XML.Enumerator.Render (renderText)
import Data.Monoid (mconcat)
import System.IO.Unsafe (unsafePerformIO)
import Yesod.Core (toSinglePiece)
import qualified Data.Map as Map

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
    go (NodeElement (Element n as children)) = go' (nameLocalName n) as $ mapM_ go children
    go _ = return ()
    go' "p" as x =
        case lookup "id" as of
            Just [ContentText t] -> [html|<p .hascomments #comment-#{toSinglePiece topic}-#{t}>#{x}|]
            _ -> [html|<p>#{x}|]
    go' "image" as x =
        case lookup "href" as of
            Just [ContentText t] -> [html|<img src=#{toLink t}>|]
            _ -> x
    go' "xref" as x =
        case lookup "href" as of
            Just [ContentText t] -> [html|<a href=#{toLink t}>#{x}|]
            _ -> x
    go' "example" _ x = [html|<section>
\#{x}|]
    go' "apiname" _ x = [html|<a href="http://hackage.haskell.org/package/#{x}">#{x}|]
    go' "codeblock" _ x = [html|<pre>
    <code>#{x}|]
    go' "note" as x =
        case lookup "type" as of
            Just [ContentText nt]
                | nt == "other" ->
                    case lookup "othertype" as of
                        Just [ContentText ot] -> [html|<aside .note-#{ot}>#{x}|]
                        _ -> [html|<aside .note-#{x}>|]
                | otherwise -> [html|<aside .#{nt}>#{x}|]
            _ -> [html|<aside .note>#{x}|]
    go' n _ x
        | n `Set.member` ditaDrop = [html||]
        | n `Set.member` ditaIgnore = x
        | n `Set.member` ditaUnchanged = [html|\<#{n}>#{x}</#{n}>|]
    go' n _ x =
        case Map.lookup n ditaMap of
            Just (n', Nothing) ->
                [html|\<#{n'}>#{x}</#{n'}>|]
            Just (n', Just c) ->
                [html|\<#{n'} .#{c}>#{x}</#{n'}>|]
            Nothing ->
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
    ]
  where
    elem' x = (x, Nothing)
    div' x = ("div", Just x)
