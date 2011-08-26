{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.UploadDitamap
    ( postUploadDitamapR
    , postUploadDitamapUrlR
    ) where

import Wiki hiding (joinPath, get)
import qualified Wiki as W
import Codec.Archive.Zip
import Data.XML.Types
import Data.ByteString (ByteString)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe, catMaybes)
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
import Control.Monad.Trans.State
import qualified Data.Set as Set
import Data.ByteString.Base64 (encode)
import Util (validateDita)
import Data.Char (isSpace)
import Network.HTTP.Enumerator (simpleHttp)
import Handler.Search (updateTerms)
import Control.Monad (guard)
import Data.List (isPrefixOf)

newtype AbsPath = AbsPath FilePath
    deriving (Ord, Show, Eq)

newtype RelPath = RelPath Text

joinPath :: AbsPath -> RelPath -> AbsPath
joinPath (AbsPath fp) (RelPath t) =
    AbsPath $ intercalate "/" pieces
  where
    exceptLast [] = []
    exceptLast x = init x
    pieces = clean id $ exceptLast pieces1 ++ pieces2
    pieces1 = split fp
    pieces2 = split $ unpack t
    split "" = []
    split x =
        let (y, z) = break (== '/') x
         in y : split (drop 1 z)
    clean front [] = front []
    clean front ("..":xs) = clean (front . (:) "..") xs
    clean front (_:"..":xs) = clean id $ front xs
    clean front (x:xs) = clean (front . (:) x) xs

data Tree = Tree
    { _treeFile :: AbsPath
    , _treeChildren :: [Tree]
    }
    deriving Show

data File = MapFile
    { _fileTitle :: Text
    , _fileSlug :: MapNodeSlug
    , _fileTree :: [Tree]
    } | DitaFile
    { _fileTitle :: Text
    , _fileSlug :: MapNodeSlug
    , _fileType :: TopicFormat
    , _fileDita :: [Node]
    } | StaticFile
    { _fileMime :: ByteString
    , _fileContents :: ByteString
    }
    deriving Show

data FileId = FIMap TMapId MapNodeSlug | FITopic TopicId MapNodeSlug | FIStatic StaticContentId

postUploadDitamapUrlR :: Handler ()
postUploadDitamapUrlR = do
    uid <- requireAuthId
    url <- runInputPost $ ireq urlField "url"
    file <- liftIO $ simpleHttp $ unpack url
    doUpload uid file

postUploadDitamapR :: Handler ()
postUploadDitamapR = do
    uid <- requireAuthId
    (_, files) <- runRequestBody
    maybe notFound (doUpload uid . fileContent) $ lookup "zip" files

doUpload :: UserId -> L.ByteString -> Handler ()
doUpload uid file = do
    let contents = fmap catMaybes $ mapM toFile $ zEntries $ toArchive file
    now <- liftIO getCurrentTime
    runDB $ do
        m <- Map.fromList <$> mapM (getId now uid) (evalState contents Set.empty)
        mapM_ (uploadContent now uid m) $ Map.toList m
    setMessageI MsgDitaMapUploaded
    redirect RedirectTemporary SettingsR

uploadContent :: UTCTime -> UserId -> Map.Map AbsPath (File, FileId) -> (AbsPath, (File, FileId)) -> YesodDB Wiki Wiki ()
uploadContent now uid m (_, (f, fid)) =
    case (f, fid) of
        (MapFile _ _ trees, FIMap mid _) -> do
            let go parent (pos, Tree topic children) =
                    case Map.lookup topic m of
                        Just (_, FITopic tid slug) -> do
                            me <- insert $ TMapNode mid parent pos (Just tid) Nothing Nothing slug
                            mapM_ (go $ Just me) $ zip [1..] children
                        Just (_, FIMap submap (MapNodeSlug slug)) -> do
                            _ <- insert $ TMapNode mid parent pos Nothing (Just submap) Nothing (MapNodeSlug $ T.append slug $ toSinglePiece submap)
                            return ()
                        Just (_, FIStatic _) -> do
                            lift $ $(logWarn) $ pack $ "DITA map refers to static content"
                        Nothing -> do
                            lift $ $(logWarn) $ pack $ "Could not find: " ++ show topic
                            return ()
            mapM_ (go Nothing) $ zip [1..] trees
        (DitaFile _ _ format dita, FITopic tid _) -> do
            text <- run_ $ enumList 8 (goN m dita []) $$ joinI $ renderText $$ consume
            let tc = TopicContent tid uid Nothing now format $ validateDita $ mconcat text
            _ <- insert tc
            updateTerms tc
        _ -> return ()

goE :: Map.Map AbsPath (File, FileId) -> Element -> [Event] -> [Event]
goE m (Element name as ns) =
    case lookup "conref" as of
        Just [ContentText conref] ->
            case resolveConref m conref of
                Just e -> goE m e
                Nothing -> defRes
        _ -> defRes
  where
    defRes =
          (EventBeginElement name as' :)
        . goN m inside
        . (EventEndElement name :)
    asPairs = map fixA as
    as' = map fst asPairs
    inside =
        case mapMaybe snd asPairs of
            title:_
                | all isWhitespace ns -> [NodeContent $ ContentText title]
            _ -> ns
    isWhitespace (NodeContent (ContentText t)) = T.all isSpace t
    isWhitespace (NodeComment _) = True
    isWhitespace (NodeInstruction _) = True
    isWhitespace _ = False
    fixA ("href", [ContentText t]) =
        (("href", [ContentText $ mappend file' rest]), x)
      where
        (file, rest) = T.break (== '#') t
        (file', x) =
            case Map.lookup (AbsPath $ unpack file) m of
                Just (DitaFile title _ _ _, FITopic tid _) -> (mconcat ["yw://topic/", toSinglePiece tid], Just title)
                Just (_, FITopic _ _) -> error "Problem in Handler.UploadDitamap.goE: This should never happen"
                Just (_, FIStatic sid) -> (mconcat ["yw://static/", toSinglePiece sid], Nothing)
                Just (_, FIMap mid _) -> (mconcat ["yw://map/", toSinglePiece mid], Nothing)
                Nothing -> (file, Nothing)
    fixA x = (x, Nothing)

resolveConref :: Map.Map AbsPath (File, FileId) -> Text -> Maybe Element
resolveConref m conref = do
    (DitaFile _ _ _ dita, _) <- Map.lookup (AbsPath $ unpack file) m
    --Element _ _ topic <- findById topicid dita
    findById elemId dita -- topic -- FIXME support case if this or topicid are missing?
  where
    (file, rest) = T.break (== '#') conref
    (_topicid, rest') = T.break (== '/') $ T.drop 1 rest
    elemId = T.drop 1 rest'

    findById _ [] = Nothing
    findById i (NodeElement e@(Element _ as cs):ns)
        | lookup "id" as == Just [ContentText i] = Just e
        | otherwise =
            case findById i cs of
                Just x -> return x
                Nothing -> findById i ns
    findById i (_:ns) = findById i ns

goN :: Map.Map AbsPath (File, FileId) -> [Node] -> [Event] -> [Event]
goN _ [] = id
goN m [x] = goN' m x
goN m (x:xs) = goN' m x . goN m xs

goN' :: Map.Map AbsPath (File, FileId) -> Node -> [Event] -> [Event]
goN' m (NodeElement e) = goE m e
goN' _ (NodeInstruction i) = (EventInstruction i :)
goN' _ (NodeContent c) = (EventContent c :)
goN' _ (NodeComment t) = (EventComment t :)

getId :: UTCTime -> UserId -> (AbsPath, File) -> YesodDB Wiki Wiki (AbsPath, (File, FileId))
getId now uid (ap@(AbsPath fp), f) = do
    fid <- go f
    return (ap, (f, fid))
  where
    fn = reverse $ takeWhile (/= '/') $ reverse fp
    go (StaticFile m c) = fmap FIStatic $ insert $ StaticContent m $ encode c
    go (MapFile title slug _) = fmap (flip FIMap slug) $ insert $ TMap uid title now
    go (DitaFile title slug _ _) = do
        mtid <- return $ do
            let topicDash = "topic-"
            guard $ topicDash `isPrefixOf` fn
            let fp' = reverse $ drop (length topicDash) fn
            let ditaBack = "atid."
            guard $ ditaBack `isPrefixOf` fp'
            let fp'' = reverse $ drop (length ditaBack) fp'
            fromSinglePiece $ T.pack fp''
        mtid' <-
            case mtid of
                Nothing -> return Nothing
                Just tid -> do
                    mt <- W.get tid
                    case mt of
                        Just t
                            | topicOwner t == uid -> return $ Just tid
                        _ -> return Nothing
        case mtid' of
            Nothing -> fmap (flip FITopic slug) $ insert (TFamily now) >>= insert . (flip (Topic uid title now) False)
            Just tid -> return $ FITopic tid slug

toFile :: Entry -> State (Set.Set MapNodeSlug) (Maybe (AbsPath, File))
toFile entry
    | ".ditamap" `isSuffixOf` fp = return $
        case parseLBS contents decodeEntities of
            Left _ -> Nothing -- FIXME
            Right x -> Just (abspath, parseMap abspath x)
    | ".dita" `isSuffixOf` fp || ".xml" `isSuffixOf` fp =
        case parseLBS contents decodeEntities of
            Left _ -> return Nothing -- FIXME
            Right x -> do
                d <- parseDita abspath x
                return $ Just (abspath, d)
    | otherwise = return $
        case lookup exten mimes of
            Nothing -> Nothing
            Just mt -> Just (abspath, StaticFile mt $ S.concat $ L.toChunks contents)
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
parseMap abspath (Document _ (Element _ asMap children) _) =
    MapFile title (MapNodeSlug theId) tree
  where
    theId =
        case lookup "id" asMap of
            Just [ContentText t] -> t
            _ -> ""
    title =
        case lookup "title" asMap of
            Just [ContentText t] -> t
            _ -> go children
      where
        go [] = "Unnamed Map"
        go (NodeElement (Element "title" _ x):_) = mconcat $ map takeText x
        go (NodeElement (Element "booktitle" _ x):_) =
            go' x
          where
            go' [] = "Unnamed Bookmap"
            go' (NodeElement (Element "mainbooktitle" _ y):_) = mconcat $ map takeText y
            go' (_:ys) = go' ys
        go (_:xs) = go xs
    takeText (NodeContent (ContentText t)) = t
    takeText _ = ""
    tree =
        mapMaybe go children
      where
        go (NodeElement (Element name as children'))
            | name `elem` ["topicref", "chapter"] =
                case lookup "href" as of
                    Just [ContentText rel] -> Just $ Tree (joinPath abspath $ RelPath rel) $ mapMaybe go children'
                    _ -> Nothing
        go _ = Nothing

parseDita :: AbsPath -> Document -> State (Set.Set MapNodeSlug) File
parseDita abspath (Document _ (Element e as'' children) _) = do
    slug <- getSlug $
        case lookup "id" as'' of
            Just [ContentText t] -> t
            _ -> "untitled"
    return $ DitaFile title slug (if e == "concept" then TFDitaConcept else TFDitaTopic) tree
  where
    getSlug t = do
        s <- get
        let slug = MapNodeSlug t
        if slug `Set.member` s
            then getSlug $ T.append t "_"
            else do
                put $ Set.insert slug s
                return slug
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
            | n `elem` ["conbody", "body", "refbody", "taskbody"] = children'
        go (_:xs) = go xs
    tree =
        map go body
      where
        go (NodeElement (Element n as children')) =
            NodeElement $ Element n as' $ map go children'
          where
            as' = map fixAttr as
        go n = n
    fixAttr (n, [ContentText rel])
        | n `elem` ["href", "src", "conref"] && notAbs rel =
            (n, [ContentText $ mappend (pack joined) rest])
          where
            (path, rest) = T.break (== '#') rel
            AbsPath joined = joinPath abspath $ RelPath path
            notAbs t = not $ "/" `T.isPrefixOf` t || "http://" `T.isPrefixOf` t || "https://" `T.isPrefixOf` t || "mailto:" `T.isPrefixOf` t
    fixAttr x = x
