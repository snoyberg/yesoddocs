{-# LANGUAGE OverloadedStrings #-}
import OldBook
import Dita
import qualified Data.Text as T
import Data.Char (toLower)
import Data.XML.Types
import Data.Either (lefts, rights)

main :: IO ()
main = loadBook >>= writeDitaMap "book/yesodbook.ditamap" . convert

convert :: Book -> DitaMap
convert b = DitaMap
    { mapTitle = bookTitle b
    , mapTopics = map (partToTopic b) $ bookParts b
    }

partToTopic :: Book -> Part -> Topic
partToTopic b p = Topic
    { topicTitle = partTitle p
    , topicId = piece
    , topicPath = [piece]
    , topicContent = []
    , topicChildren = map (chapterToTopic b ((:) piece)) $ partChapters p
    }
  where
    piece = T.map toLower $ partTitle p

type Endo = [T.Text] -> [T.Text]

chapterToTopic :: Book -> Endo -> Chapter -> Topic -- FIXME chapter status
chapterToTopic b front c = Topic
    { topicTitle = chapterTitle c
    , topicId = chapterSlug c
    , topicPath = front' []
    , topicContent = map (blockToNode b $ length $ front' []) $ chapterIntro c
    , topicChildren = map (sectionToChapter b front') $ chapterSections c ++ summary
    }
  where
    front' = front . (:) (chapterSlug c)
    summary =
        case chapterSummary c of
            Nothing -> []
            Just bs -> [Section "summary" "Summary" $ map Right bs]

toTop :: Int -> T.Text
toTop i = T.concat $ replicate i "../"

blockToNode :: Book -> Int -> Block -> Node
blockToNode b i (Paragraph pid inlines) =
    NodeElement $ Element "p" [("id", [ContentText pid])] $ map (inlineToNode b i) inlines
blockToNode b i (UList lis) = NodeElement $ Element "ul" [] $ map (liToNode b i) lis
blockToNode b i (OList lis) = NodeElement $ Element "ol" [] $ map (liToNode b i) lis
blockToNode b _ (CodeBlock t) = NodeElement $ Element "codeblock" [] [NodeContent $ ContentText t]
blockToNode b i (Snippet content) = NodeElement $ Element "codeblock" [("outputclass", [ContentText "haskell"])] [NodeContent $ ContentText content]
blockToNode b i (Advanced bs) = NodeElement $ Element "note" [("type", [ContentText "other"]), ("othertype", [ContentText "advanced"])] $ map (blockToNode b i) bs
blockToNode b i (Note bs) = NodeElement $ Element "note" [] $ map (inlineToNode b i) bs
blockToNode b i (Image src title) = NodeElement $ Element "fig" []
    [ NodeElement $ Element "title" [] [NodeContent $ ContentText title]
    , NodeElement $ Element "image" [("href", [ContentText $ T.concat [toTop i, "images/", src]])] []
    ]
blockToNode b i (Defs ds) = NodeElement $ Element "dl" [] $ map (defToNode b i) ds
blockToNode _ i (Example t) = NodeElement $ Element "codeblock" [("outputclass", [ContentText "lhaskell"])] [NodeContent $ ContentText t]

defToNode :: Book -> Int -> (T.Text, [Inline]) -> Node
defToNode b i (key, value) = NodeElement $ Element "dlentry" []
    [ NodeElement $ Element "dt" [] [NodeContent $ ContentText key]
    , NodeElement $ Element "dd" [] $ map (inlineToNode b i) value
    ]

inlineToNode :: Book -> Int -> Inline -> Node
inlineToNode _ _ (Inline t) = NodeContent $ ContentText t
inlineToNode b i (Emphasis t) = NodeElement $ Element "i" [] $ map (inlineToNode b i) t
inlineToNode b i (Strong t) = NodeElement $ Element "b" [] $ map (inlineToNode b i) t
inlineToNode _ _ (Term t) = NodeElement $ Element "term" [] [NodeContent $ ContentText t]
inlineToNode _ _ (Hackage t) = NodeElement $ Element "apiname" [] [NodeContent $ ContentText t]
inlineToNode _ _ (Xref h t) = NodeElement $ Element "xref"
    [ ("href", [ContentText h])
    , ("scope", [ContentText "external"])
    , ("format", [ContentText "html"])
    ] [NodeContent $ ContentText t]
inlineToNode _ _ (Code t) = NodeElement $ Element "codeph" [] [NodeContent $ ContentText t]
inlineToNode book i (Link chap msec t) =
    NodeElement $ Element "xref"
        [ ("href", [ContentText $ T.concat [toTop $ i - 1, getLink book chap msec]])
        ] [NodeContent $ ContentText t]
inlineToNode _ _ (Abbr title t) = NodeContent $ ContentText $ T.concat
    [ t
    , " ("
    , t
    , ")"
    ]

liToNode :: Book -> Int -> ListItem -> Node
liToNode b i = NodeElement . Element "li" [] . map (inlineToNode b i) . unListItem

getLink :: Book -> T.Text -> Maybe T.Text -> T.Text
getLink b cslug ms =
    go $ bookParts b
  where
    go [] = error $ "Link not found: " ++ show (cslug, ms)
    go (p:ps) =
        case goP p $ partChapters p of
            Nothing -> go ps
            Just t -> t
    goP _ [] = Nothing
    goP p (c:cs) =
        if chapterSlug c == cslug
            then Just $ goC p c
            else goP p cs
    goC p c =
        case ms of
            Nothing -> T.concat [T.map toLower $ partTitle p, "/", chapterSlug c, ".dita"]
            Just "join-path" -> T.concat [T.map toLower $ partTitle p, "/", chapterSlug c, "/rendering-parsing-urls/join-path.dita"]
            Just s -> T.concat [T.map toLower $ partTitle p, "/", chapterSlug c, "/", s, ".dita"]

sectionToChapter :: Book -> Endo -> Section -> Topic
sectionToChapter b front s = Topic
    { topicTitle = sectionTitle s
    , topicId = sectionId s
    , topicPath = front' []
    , topicContent = map (blockToNode b $ length $ front' []) $ rights $ sectionBlocks s
    , topicChildren = map (sectionToChapter b front') $ lefts $ sectionBlocks s
    }
  where
    front' = front . (:) (sectionId s)
