{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.UploadDitamap
    ( postUploadDitamapR
    ) where

import Wiki hiding (joinPath)
import Codec.Archive.Zip
import Data.XML.Types
import Data.ByteString (ByteString)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe, fromJust)
import Text.XML.Enumerator.Document (parseLBS)
import Text.XML.Enumerator.Render (renderText)
import Text.XML.Enumerator.Parse (decodeEntities)
import Data.Monoid (mconcat)
import Data.List (isSuffixOf, intercalate)
import Data.Text (unpack, pack)
import qualified Data.Text as T
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Data.Enumerator (run_, enumList, ($$), joinI)
import Data.Enumerator.List (consume)

newtype AbsPath = AbsPath FilePath
    deriving (Ord, Show, Eq)

newtype RelPath = RelPath Text

joinPath :: AbsPath -> RelPath -> AbsPath
joinPath (AbsPath fp) (RelPath t) =
    AbsPath $ intercalate "/" pieces
  where
    pieces = clean $ pieces1 ++ pieces2
    pieces1 = split fp
    pieces2 = split $ unpack t
    split "" = []
    split x =
        let (y, z) = break (== '/') x
         in y : split (drop 1 z)
    clean [] = []
    clean ("..":xs) = ".." : clean xs
    clean (_:"..":xs) = clean xs
    clean (x:xs) = x : clean xs

data Tree = Tree
    { _treeFile :: AbsPath
    , _treeChildren :: [Tree]
    }
    deriving Show

data File = MapFile
    { _fileTitle :: Text
    , _fileTree :: [Tree]
    } | DitaFile
    { _fileTitle :: Text
    , _fileType :: TopicFormat
    , _fileDita :: [Node]
    } | StaticFile
    { _fileMime :: ByteString
    , _fileContents :: ByteString
    }
    deriving Show

data FileId = FIMap TMapId | FITopic TopicId | FIStatic StaticContentId

postUploadDitamapR :: Handler ()
postUploadDitamapR = do
    uid <- requireAuthId
    (_, files) <- runRequestBody
    file <- maybe notFound (return . fileContent) $ lookup "zip" files
    let contents = mapMaybe toFile $ zEntries $ toArchive file
    now <- liftIO getCurrentTime
    runDB $ do
        m <- Map.fromList <$> mapM (getId now uid) contents
        mapM_ (uploadContent now uid m) $ Map.toList m
    setMessageI MsgDitaMapUploaded
    redirect RedirectTemporary SettingsR

uploadContent :: UTCTime -> UserId -> Map.Map AbsPath (File, FileId) -> (AbsPath, (File, FileId)) -> YesodDB Wiki (GGHandler Wiki Wiki IO) ()
uploadContent now uid m (_, (f, fid)) =
    case (f, fid) of
        (MapFile _ trees, FIMap mid) -> do
            let go parent (pos, Tree topic children) =
                    case Map.lookup topic m of
                        Just (_, FITopic tid) -> do
                            me <- insert $ TMapNode mid parent pos (Just tid) Nothing Nothing
                            mapM_ (go $ Just me) $ zip [1..] children
                        _ -> return ()
            mapM_ (go Nothing) $ zip [1..] trees
        (DitaFile _ format dita, FITopic tid) -> do
            text <- run_ $ enumList 8 (goN m dita []) $$ joinI $ renderText $$ consume
            _ <- insert $ TopicContent tid uid Nothing now format $ mconcat text
            return ()
        _ -> return ()

goE :: Map.Map AbsPath (File, FileId) -> Element -> [Event] -> [Event]
goE m (Element name as ns) =
      (EventBeginElement name (map fixA as) :)
    . goN m ns
    . (EventEndElement name :)
  where
    fixA ("href", [ContentText t]) =
        ("href", [ContentText $ mappend file' rest])
      where
        (file, rest) = T.break (== '#') t
        file' =
            case Map.lookup (AbsPath $ unpack file) m of
                Just (_, FITopic tid) -> mconcat ["yw://topic/", toSinglePiece tid]
                Just (_, FIStatic sid) -> mconcat ["yw://static/", toSinglePiece sid]
                Just (_, FIMap mid) -> mconcat ["yw://map/", toSinglePiece mid]
                Nothing -> file
    fixA x = x

goN :: Map.Map AbsPath (File, FileId) -> [Node] -> [Event] -> [Event]
goN _ [] = id
goN m [x] = goN' m x
goN m (x:xs) = goN' m x . goN m xs

goN' :: Map.Map AbsPath (File, FileId) -> Node -> [Event] -> [Event]
goN' m (NodeElement e) = goE m e
goN' _ (NodeInstruction i) = (EventInstruction i :)
goN' _ (NodeContent c) = (EventContent c :)
goN' _ (NodeComment t) = (EventComment t :)

getId :: UTCTime -> UserId -> (AbsPath, File) -> YesodDB Wiki (GGHandler Wiki Wiki IO) (AbsPath, (File, FileId))
getId now uid (ap, f) = do
    fid <- go f
    return (ap, (f, fid))
  where
    go (StaticFile _m _c) = return $ FIStatic $ fromJust $ fromSinglePiece "0" -- FIXME fmap FIStatic $ insert $ StaticContent m c
    go (MapFile title _) = fmap FIMap $ insert $ TMap uid title now
    go (DitaFile title _ _) = fmap FITopic $ insert (TFamily now) >>= insert . Topic uid title now

toFile :: Entry -> Maybe (AbsPath, File)
toFile entry
    | ".ditamap" `isSuffixOf` fp =
        case parseLBS contents decodeEntities of
            Left _ -> Nothing -- FIXME
            Right x -> Just (abspath, parseMap abspath x)
    | ".dita" `isSuffixOf` fp || ".xml" `isSuffixOf` fp =
        case parseLBS contents decodeEntities of
            Left _ -> Nothing -- FIXME
            Right x -> Just (abspath, parseDita abspath x)
    | otherwise =
        case lookup exten mimes of
            Nothing -> Nothing
            Just mt -> Just (abspath, StaticFile mt $ S.concat $ L.toChunks contents)
    | otherwise = Nothing
  where
    mimes =
        [ ("png", "image/png")
        , ("gif", "image/gif")
        , ("jpeg", "image/jpeg")
        , ("jpg", "image/jpeg")
        ]
    exten = reverse $ takeWhile (/= '.') $ reverse fp
    contents = fromEntry entry
    fp = eRelativePath entry
    abspath = AbsPath fp

parseMap :: AbsPath -> Document -> File
parseMap abspath (Document _ (Element _ _ children) _) =
    MapFile title tree
  where
    title =
        go children
      where
        go [] = "Unnamed Map"
        go (NodeElement (Element "title" _ x):_) = mconcat $ map takeText x
        go (_:xs) = go xs
    takeText (NodeContent (ContentText t)) = t
    takeText _ = ""
    tree =
        mapMaybe go children
      where
        go (NodeElement (Element "topicref" as children')) =
            case lookup "href" as of
                Just [ContentText rel] -> Just $ Tree (joinPath abspath $ RelPath rel) $ mapMaybe go children'
                _ -> Nothing
        go _ = Nothing

parseDita :: AbsPath -> Document -> File
parseDita abspath (Document _ (Element e _ children) _) =
    DitaFile title (if e == "concept" then TFDitaConcept else TFDitaTopic) tree
  where
    title =
        go children
      where
        go [] = "Unnamed Topic"
        go (NodeElement (Element "title" _ x):_) = mconcat $ map takeText x
        go (_:xs) = go xs
    takeText (NodeContent (ContentText t)) = t
    takeText _ = ""
    body =
        go children
      where
        go [] = []
        go (NodeElement (Element n _ children'):_)
            | n `elem` ["conbody", "body"] = children'
        go (_:xs) = go xs
    tree =
        map go body
      where
        go (NodeElement (Element n as children')) =
            NodeElement $ Element n as' children'
          where
            as' = map fixAttr as
        go n = n
    fixAttr (n, [ContentText rel])
        | n `elem` ["href", "src"] =
            (n, [ContentText $ mappend (pack joined) rest])
          where
            (path, rest) = T.break (== '#') rel
            AbsPath joined = joinPath abspath $ RelPath path
    fixAttr x = x
