{-# LANGUAGE QuasiQuotes #-}
import Text.Hamlet
import Book
import qualified Data.ByteString.Lazy as L

main :: IO ()
main = do
    book <- loadBook
    L.putStr $ renderHtml [$xhamlet|\
\<?xml version="1.0" encoding="utf-8"?>
<root xmlns="http://www.w3.org/1999/XSL/Format">
    <layout-master-set>
        <simple-page-master master-name="cover">
            <region-body margin="2in">
        <simple-page-master master-name="body-odd">
            <region-body margin="1in">
            <region-before region-name="header-odd" extent="1in">
            <region-after region-name="footer" extent="1in">
        <simple-page-master master-name="body-even">
            <region-body margin="1in">
            <region-before region-name="header-even" extent="1in">
            <region-after region-name="footer" extent="1in">
        <page-sequence-master master-name="body">
            <repeatable-page-master-alternatives>
                <conditional-page-master-reference odd-or-even="even" master-reference="body-even">
                <conditional-page-master-reference odd-or-even="odd" master-reference="body-odd">
    <page-sequence master-reference="cover">
        <flow flow-name="xsl-region-body" text-align="center">
            <block font-size="48pt">#{bookTitle book}
            <block font-size="36pt">Michael snoyman
    $forall p <- bookParts book
        <page-sequence master-reference="cover">
            <flow flow-name="xsl-region-body" text-align="center">
                <block font-size="36pt" text-align="center" break-before="odd-page">
                    \#{partTitle p}
        $forall c <- partChapters p
            <page-sequence master-reference="body">
                <static-content flow-name="header-even">
                    <block text-align-last="justify" margin="0.2in 0.5in">
                        <page-number>
                        <leader>
                        \#{bookTitle book}
                <static-content flow-name="header-odd">
                    <block text-align-last="justify" margin="0.2in 0.5in">
                        \#{bookTitle book}
                        <leader>
                        <page-number>
                <static-content flow-name="xsl-footnote-separator">
                    <block>
                        <leader leader-pattern="rule" leader-length="100%" rule-style="solid" rule-thickness="0.5pt">
                <flow flow-name="xsl-region-body" text-align="justify">
                    <block border-bottom="1px solid black" font-size="20pt" break-before="odd-page">
                        \#{chapterTitle c}
                    $forall b <- chapterIntro c
                        \^{block b}
                    $forall s <- chapterSections c
                        \^{section h1 s}
                    $maybe s <- chapterSummary c
                        <block font-size="#{show h1}pt" space-before="20pt">Summary
                        $forall b <- s
                            \^{block b}
|]
  where
    h1 = 20

section :: Int -> Section -> Html
section h1 (Section _ title sbs) = [$xhamlet|\
<block font-size="#{show h1}pt" space-before="20pt">#{title}
$forall bs <- sbs
    \^{sectionBlock bs}
|]
  where
    sectionBlock (Left s) = section (h1 - 2) s
    sectionBlock (Right b) = block b

block :: Block -> Html
block (Paragraph _ is) = [$xhamlet|\
<block space-before="10pt" space-after="10pt" text-indent="0.3in">
    $forall i <- is
        \^{inline i}
|]
block (UList lis) = [$xhamlet|\
<list-block provisional-distance-between-starts="18pt" provisional-label-separation="3pt" space-before="10pt" space-after="10pt">
    $forall li <- lis
        <list-item>
            <list-item-label end-indent="label-end()">
                <block>&#x2022;
            <list-item-body start-indent="body-start()">
                <block>
                    $forall i <- unListItem li
                        \^{inline i}
|]
block (OList lis) = [$xhamlet|\
<list-block provisional-distance-between-starts="18pt" provisional-label-separation="3pt" space-before="10pt" space-after="10pt">
    $forall li <- zip' lis
        <list-item>
            <list-item-label end-indent="label-end()">
                <block>#{show (fst li)}.
            <list-item-body start-indent="body-start()">
                <block>
                    $forall i <- unListItem (snd li)
                        \^{inline i}
|]
  where
    zip' = zip [1 :: Int ..]
block (CodeBlock text) = [$xhamlet|\
<block font-family="monospace" linefeed-treatment="preserve" white-space-treatment="preserve" white-space-collapse="false">
    \#{text}
|]
block (Advanced bs) = [$xhamlet|\
<block margin="10pt 0 10pt 10pt" background-color="#eee">
    $forall b <- bs
        \^{block b}
|]
block (Note is) = [$xhamlet|\
<block space-before="10pt" space-after="10pt">
    <inline font-weight="bold">Note:
    \ 
    $forall i <- is
        \^{inline i}
|]
block _ = [$xhamlet||]

inline :: Inline -> Html
inline (Inline t) = [$xhamlet|\#{t}
|]
inline (Emphasis is) = [$xhamlet|\
<inline font-style="italic">
    $forall i <- is
        \^{inline i}
|]
inline (Term t) = [$xhamlet|<inline font-weight="bold">#{t}
|]
inline (Hackage p) = [$xhamlet|\
\#{p}
<footnote>
    <inline baseline-shift="super" font-size="smaller">*
    <footnote-body>
        <block>* http://hackage.haskell.org/package/#{p}
|]
inline (Xref href inner) = [$xhamlet|\
\#{inner}
<footnote>
    <inline baseline-shift="super" font-size="smaller">*
    <footnote-body>
        <block>* #{href}
|]
inline (Code text) = [$xhamlet|<inline font-family="monospace">#{text}
|]
inline (Link _ _ text) = [$xhamlet|\#{text}
|] -- FIXME
inline (Abbr _ text) = [$xhamlet|\#{text}
|] -- FIXME
