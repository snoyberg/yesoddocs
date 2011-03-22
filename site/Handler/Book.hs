{-# LANGUAGE TypeFamilies, QuasiQuotes, TemplateHaskell, OverloadedStrings, MultiParamTypeClasses #-}
module Handler.Book where

import Yesod
import Yesod.Helpers.Static
import Yesod.Helpers.Sitemap
import Yesod.Helpers.AtomFeed
import Yesod.Form.Jquery
import Settings
import Text.Pandoc (readMarkdown, defaultParserState, writeHtmlString, defaultWriterOptions)
import Language.Haskell.HsColour hiding (string)
import Language.Haskell.HsColour.Colourise (defaultColourPrefs)
import qualified Language.Haskell.HsColour.CSS as CSS
import Entry
import Control.Arrow ((&&&))
import Data.List (groupBy, stripPrefix)
import Data.Maybe (fromMaybe)
import Data.Function (on)
import qualified System.IO.UTF8 as U
import Data.Time
import Book
import qualified Data.Text as T
import qualified Data.Text as TS -- FIXME remove
import Data.Text (Text)
import qualified Text.Highlighting.Kate as Kate
import Text.XHtml.Strict (showHtmlFragment)
import Data.Either (lefts)
import Control.Concurrent.AdvSTM
import Control.Concurrent.AdvSTM.TVar
import Comments
import Data.List (sortBy)
import Text.Blaze (toHtml)
import qualified Data.ByteString.Char8 as S8
import YesodDocs

getBookR :: Handler RepHtml
getBookR = defaultLayout $ do
    setTitle "Yesod Web Framework Book"
    book <- lift $ fmap getBook getYesod
    let unpack = T.unpack
    let chapters = concatMap partChapters $ bookParts book
    atomLink CommentsFeedR "Book Comments"
    addHamlet $(hamletFile "book")
    addCassius $(cassiusFile "book")

getChapterR :: String -> Handler RepHtml
getChapterR slug = do
    book <- fmap getBook getYesod
    let chapters = concatMap partChapters $ bookParts book
    chapter <-
        case filter (\x -> chapterSlug x == T.pack slug) chapters of
            [] -> notFound
            x:_ -> return x
    let previous = getPrev chapters
    let next = getNext chapters
    y <- getYesod
    let title = T.unpack $ chapterTitle chapter
    let unpack = T.unpack
    tcs <- fmap comments getYesod
    cs' <- liftIO $ atomically $ readTVar tcs
    let cs = fromMaybe [] $ lookup slug cs'
    let html = chapterToHtml cs chapter
    defaultLayout $ do
        setTitle $ string $ "Yesod Book: " ++ title
        addScriptEither $ urlJqueryJs y
        atomLink CommentsFeedR "Book Comments"
        addHamlet $(hamletFile "chapter")
        addCassius $(cassiusFile "book")
        addStylesheet $ StaticR hk_kate_css
        addJulius $(juliusFile "chapter")
        addCassius $(cassiusFile "chapter")
        addStylesheet $ StaticR hscolour_css
        -- addScript $ StaticR hyphenate_js
  where
    slug' = T.pack slug
    getPrev (x:yc@(Chapter { chapterSlug = y }):rest)
        | y == slug' = Just x
        | otherwise = getPrev $ yc : rest
    getPrev _ = Nothing
    getNext ((Chapter { chapterSlug = x }):y:rest)
        | x == slug' = Just y
        | otherwise = getNext $ y : rest
    getNext _ = Nothing
    go ('~':filename) = snippet filename
    go x = return x

snippet :: String -> IO String
snippet filename = do
    raw <- U.readFile $ "snippets/" ++ filename ++ ".hs"
    let raw' = unlines $ go False $ lines raw
    return $ "<pre class='code-block'><code>" ++ (unlines $ zipWith go' [1..] $ lines
           $ CSS.hscolour False raw')
  where
    go _ [] = []
    go False ("-- START":rest) = go True rest
    go True ("-- STOP":rest) = go False rest
    go False (_:rest) = go False rest
    go True (x:rest) = x : go True rest
    go' _ "</pre>" = "</code></pre>"
    go' i s = concat
        [ "<span class='line-number'>"
        , show (i :: Int)
        , "</span>"
        , removePre s
        ]
    removePre s = fromMaybe s $ stripPrefix "<pre>" s

chapterToHtml :: [(Text, [Comment])] -> Chapter -> Hamlet YesodDocsRoute
chapterToHtml cs c@(Chapter { chapterIntro = intro, chapterSections = sections
                       , chapterSummary = msummary
                       , chapterSynopsis = msynopsis }) = [$hamlet|\
$if not (null sections)
    <div id="toc">
        <ul>
            <li>
                <a href="#introduction">Introduction
            $maybe _ <- msynopsis
                <a href=#synopsis>Synopsis
            $forall s <- sections
                \^{sectionToc s}
            $maybe _ <- msummary
                <li>
                    <a href="#summary">Summary
<div id="introduction">
    $forall b <- intro
        \^{blockToHtml c cs b}
$maybe synopsis <- msynopsis
    <h2 #synopsis>Synopsis
    #{preEscapedString $ showHtmlFragment $ formatKateLines synopsis}
$forall s <- sections
    <h2 id="#{sectionId s}">#{sectionTitle s}
    $forall b <- sectionBlocks s
        \^{sectionBlockToHtml c cs firstLevel b}
$maybe summary <- msummary
    <h2 id="summary">Summary
    $forall b <- summary
        \^{blockToHtml c cs b}
|]
  where
    firstLevel = 3

sectionToc :: Section -> Hamlet YesodDocsRoute
sectionToc s = [$hamlet|\
<li>
    <a href="##{sectionId s}">#{sectionTitle s}
    $if not (null (lefts (sectionBlocks s)))
        <ul>
            $forall s <- lefts (sectionBlocks s)
                \^{sectionToc s}
|]

sectionBlockToHtml :: Chapter -> [(Text, [Comment])]
                   -> Int -> Either Section Block -> Hamlet YesodDocsRoute
sectionBlockToHtml chap cs level (Left section) = [$hamlet|\
<div class="section#{show level}">
    \^{showTitle (sectionId section) level (sectionTitle section)}
    $forall b <- sectionBlocks section
        \^{sectionBlockToHtml chap cs nextLevel b}
|]
  where
    showTitle :: T.Text -> Int -> T.Text -> Hamlet YesodDocsRoute
    showTitle i 3 t = [$hamlet|<h3 id="#{i}">#{t}
|]
    showTitle i 4 t = [$hamlet|<h4 id="#{i}">#{t}
|]
    showTitle i 5 t = [$hamlet|<h5 id="#{i}">#{t}
|]
    showTitle i _ t = [$hamlet|<h6 id="#{i}">#{t}
|]
    nextLevel = level + 1
sectionBlockToHtml chap cs _ (Right b) = blockToHtml chap cs b

blockToHtml :: Chapter -> [(Text, [Comment])] -> Block -> Hamlet YesodDocsRoute -- FIXME put one of those paragraph symbol links here
blockToHtml chap chapCs (Paragraph pid is) = [$hamlet|
<p id="#{pid}">
    <span .permalink
        <a href=##{pid}>&#x204B
    $forall i <- is
        \^{inlineToHtml i}
    <span .comment-count>#{csCount'}
<div .comments>
    $forall c <- cs
        <div .comment>
            <span .name>#{commentName c}
            <span .datetime>#{show (utctDay (commentTime c))}
            <div .content>#{commentContent c}
    <form method="post" action="@{CommentR (TS.unpack (chapterSlug chap)) $ TS.pack $ T.unpack pid}">
        <b>Add a comment
        \Name: 
        <input type="text" name="name" .name>
        <textarea name="content">
        <input type="submit" value="Add comment">
|]
  where
    unpack = TS.unpack
    cs = fromMaybe [] $ lookup (T.pack $ TS.unpack pid) chapCs
    csCount = length cs
    csCount'
        | csCount == 0 = "No comments"
        | csCount == 1 = "1 comment"
        | otherwise = show csCount ++ " comments"
blockToHtml _ _ (Note is) = [$hamlet|\
<p .note>
    \Note: 
    $forall i <- is
        \^{inlineToHtml i}
|]
blockToHtml _ _ (UList items) = [$hamlet|\
<ul>
    $forall i <- items
        \^{listItemToHtml i}
|]
blockToHtml _ _ (OList items) = [$hamlet|\
<ol>
    $forall i <- items
        \^{listItemToHtml i}
|]
blockToHtml _ _ (CodeBlock c) = [$hamlet|\
<code>
    <pre>#{c}
|]
blockToHtml _ _ (Snippet s) = [$hamlet|\
\#{preEscapedString $ showHtmlFragment $ formatKateLines s}
|]
blockToHtml chap cs (Advanced bs) = [$hamlet|\
<div .advanced>
    $forall b <- bs
        \^{blockToHtml chap cs b}
|]
blockToHtml _ _ (Image { imageSrc = src, imageTitle = title }) = [$hamlet|\
<div .image>
    <img src="/static/book/#{src}" alt="#{title}" title="#{title}">
    \#{title}
|]
blockToHtml _ _ (Defs ds) = [$hamlet|\
<table border="1">
    $forall d <- ds
        <tr>
            <th>#{fst d}
            <td>
                $forall i <- snd d
                    \^{inlineToHtml i}
|]
blockToHtml _ _ (Markdown text) =
    [$hamlet|\#{content}
|]
  where
    pandoc = readMarkdown defaultParserState $ T.unpack text
    content = preEscapedString $ writeHtmlString defaultWriterOptions pandoc
blockToHtml _ _ (Example text) =
    const $ preEscapedString $ colorize "" (T.unpack text) True

listItemToHtml :: ListItem -> Hamlet YesodDocsRoute
listItemToHtml (ListItem is) = [$hamlet|\
<li>
    $forall i <- is
        \^{inlineToHtml i}
|]

inlineToHtml :: Inline -> Hamlet YesodDocsRoute
inlineToHtml (Inline t) = [$hamlet|\#{t}
|]
inlineToHtml (Emphasis is) = [$hamlet|\
<i>
    $forall i <- is
        \^{inlineToHtml i}
|]
inlineToHtml (Strong is) = [$hamlet|\
<b>
    $forall i <- is
        \^{inlineToHtml i}
|]
inlineToHtml (Term t) = [$hamlet|<b>#{t}
|]
inlineToHtml (Hackage t) = [$hamlet|\
<a href="http://hackage.haskell.org/package/#{t}">#{t}
|]
inlineToHtml (Xref href inner) = [$hamlet|<a href="#{href}">#{inner}
|]
inlineToHtml (Code inner) = [$hamlet|<code>#{inner}
|]
inlineToHtml (Link chapter msection inner) = [$hamlet|\
<a href="@{ChapterR (TS.unpack chapter)}#{section}">#{inner}
|]
  where
    section =
        case msection of
            Nothing -> ""
            Just section' -> [$hamlet|\##{section'}
|] :: Html
    unpack = T.unpack
inlineToHtml (Abbr title inner) = [$hamlet|\
<abbr title="#{title}">#{inner}
|]
