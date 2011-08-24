{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.DownloadDitamap
    ( getDownloadDitamapR
    ) where

import Wiki hiding (joinPath, get)
import Codec.Archive.Zip
import qualified Text.XML.Enumerator.Resolved as X
import qualified Data.Set as Set
import Data.Text (pack, unpack)
import Data.Text.Encoding (encodeUtf8)
import Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Text.Hamlet.XML (xml)
import Database.Persist.GenericSql (SqlPersist)
import Data.Monoid (mconcat)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

newtype ZipFile = ZipFile [Entry]
instance HasReps ZipFile where
    chooseRep (ZipFile es) _ = return ("multipart/x-zip", toContent $ fromArchive $ foldr addEntryToArchive emptyArchive es)

data LoadedFile = LFMap TMapId | LFTopic TopicId | LFStatic StaticContentId
    deriving (Eq, Ord)

data LoadState = LoadState [Entry] (Set.Set LoadedFile) [LoadedFile]

getDownloadDitamapR :: TMapId -> Handler ZipFile
getDownloadDitamapR tmid0 = do
    setHeader "Content-Disposition" $ TE.encodeUtf8 $ T.concat ["attachment; filename=map-", toSinglePiece tmid0, ".zip"]
    runDB . load . LoadState [] Set.empty . return . LFMap $ tmid0
  where
    load (LoadState es _ []) = return $ ZipFile es
    load (LoadState es loaded (lf:lfs)) = do
        (fp, bs, toload) <- load' lf
        let loaded' = Set.insert lf loaded
        let toload' = filter (\x -> not $ x `Set.member` loaded') toload
        load $ LoadState (toEntry fp 0 (L.fromChunks [bs]) : es) loaded' $ toload' ++ lfs
    load' (LFStatic sid) = do
        StaticContent mt c <- get404 sid
        let fp = staticFileName (toSinglePiece sid) mt
        return (fp, c, [])
    load' (LFTopic tid) = do
        tcs <- selectList [TopicContentTopic ==. tid] [LimitTo 1, Desc TopicContentChanged]
        topic <- get404 tid
        lbs <-
            case tcs of
                [] -> return $ "<topic><title>No content found</title></topic>"
                (_, tc):_ -> return $ toLBS $
                    case topicContentFormat tc of
                        TFMarkdown -> (topicdtd, foreign "markdown" topic tc)
                        TFHtml -> (topicdtd, foreign "html" topic tc)
                        TFText -> (topicdtd, foreign "text" topic tc)
                        TFDitaConcept -> (conceptdtd, ditaConcept topic tc)
                        TFDitaTopic -> (topicdtd, ditaTopic topic tc)
        return (topicName tid, S.concat $ L.toChunks lbs, [])
      where
        topicdtd = X.Doctype "topic" $ Just $ X.PublicID "-//OASIS//DTD DITA Topic//EN" "topic.dtd"
        conceptdtd = X.Doctype "concept" $ Just $ X.PublicID "-//OASIS//DTD DITA Concept//EN" "concept.dtd"
        toLBS (dtd, [X.NodeElement e]) = X.renderLBS $ X.Document (X.Prologue [] (Just dtd) []) e []
        toLBS _ = "Internal XML error"
        foreign f topic tc = [xml|
<topic id=topic-#{toSinglePiece tid}>
    <title>#{topicTitle topic}
    <body>
        <foreign outputclass=#{f}>#{topicContentContent tc}
|]
        ditaConcept topic tc = [xml|
<concept id=topic-#{toSinglePiece tid}>
    <title>#{topicTitle topic}
    <conbody>
        ^{toNodes $ topicContentContent tc}
|]
        ditaTopic topic tc = [xml|
<topic id=topic-#{toSinglePiece tid}>
    <title>#{topicTitle topic}
    <body>
        ^{toNodes $ topicContentContent tc}
|]
        toNodes t =
            case X.parseLBS lbs X.decodeEntities of
                Left _ -> [xml|<p>Error parsing XML content.|]
                Right (X.Document _ (X.Element _ _ x) _) -> x
          where
            lbs = L.fromChunks ["<x>", encodeUtf8 t, "</x>"]
    load' (LFMap tmid) = do
        m <- get404 tmid
        (nodes, front) <- loadMap tmid Nothing id
        let nodes' = [xml|
<map id=map-#{toSinglePiece tmid}>
    <title>#{tMapTitle m}
    ^{nodes}
|]
        return (mapName tmid, S.concat $ L.toChunks $ toLBS nodes', front [])
      where
        mapdtd = X.Doctype "map" $ Just $ X.PublicID "-//OASIS//DTD DITA Map//EN" "map.dtd"
        toLBS [X.NodeElement e] = X.renderLBS $ X.Document (X.Prologue [] (Just mapdtd) []) e []
        toLBS _ = "Internal XML error"

loadMap :: TMapId -> Maybe TMapNodeId -> ([LoadedFile] -> [LoadedFile]) -> SqlPersist (GGHandler Wiki Wiki IO) ([X.Node], ([LoadedFile] -> [LoadedFile]))
loadMap tmid parent front = do
    xs <- selectList [TMapNodeMap ==. tmid, TMapNodeParent ==. parent] [Asc TMapNodePosition] >>= mapM go
    return (map fst xs, mconcat $ front : map snd xs)
  where
    go (tmnid, tmn) = do
        (inside, front') <- loadMap tmid (Just tmnid) id
        case (tMapNodeCtopic tmn, tMapNodeCmap tmn) of
            (Just tid, _) -> return (X.NodeElement $ X.Element "topicref" [("href", pack $ topicName tid)] inside, (LFTopic tid:) . front')
            (Nothing, Just submap) -> return (X.NodeElement $ X.Element "mapref" [("href", pack $ mapName submap)] inside, (LFMap submap:) . front')
            (Nothing, Nothing) -> return (X.NodeElement $ X.Element "topichead" [("navtitle", fromMaybe "Untitled" $ tMapNodeTitle tmn)] inside, front')

topicName :: TopicId -> FilePath
topicName tid = "topic-" ++ unpack (toSinglePiece tid) ++ ".dita"

mapName :: TMapId -> FilePath
mapName tmid = "map-" ++ unpack (toSinglePiece tmid) ++ ".ditamap"

staticFileName :: Text -> ByteString -> FilePath
staticFileName t mt =
    unpack t ++ '.' : ext
  where
    ext = case mt of
            "image/png" -> "png"
            "image/gif" -> "gif"
            "image/jpeg" -> "jpeg"
            _ -> "bin"
