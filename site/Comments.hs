{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}
module Comments where

import Yesod hiding (get)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8, decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Data.Time
import Data.Serialize
import System.IO.Cautious
import Prelude hiding (writeFile)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Control.Applicative
import System.Directory (doesFileExist)
import qualified Data.Object.Yaml as Y
import Data.Object
import qualified Data.Text as T

#if PRODUCTION
import Data.Object.Yaml hiding (encode, decode)
import Control.Monad (join, unless)
#endif

data Comment = Comment
    { commentName :: Text
    , commentContent :: Textarea
    , commentTime :: UTCTime
    }

type Comments = [(String, [(Text, [Comment])])]

commentsToSO :: Comments -> Object String String
commentsToSO =
    Mapping . map go
  where
    go (x, y) = (x, Mapping $ map go' y)
    go' (x, y) = (T.unpack x, Sequence $ map go'' y)
    go'' (Comment x (Textarea y) z) = Mapping
        [ ("name", Scalar $ T.unpack x)
        , ("content", Scalar $ T.unpack y)
        , ("time", Scalar $ show z)
        ]

commentsFile :: String
commentsFile = "comments.dat"

commentsYaml :: String
commentsYaml = "comments.yaml"

loadComments :: IO Comments
loadComments = do
#if PRODUCTION
    Mapping so <- join $ decodeFile commentsYaml
    mapM go (so :: [(String, Object String String)])
  where
    go :: (String, Object String String) -> IO (String, [(Text, [Comment])])
    go (x, Mapping y) = do
        y' <- mapM go' y
        return (x, y')
    go' (x, Sequence y) = do
        y' <- mapM go'' y
        return (T.pack x, y')
    go'' (Mapping m) = do
        Just (Scalar name) <- return $ lookup "name" m
        Just (Scalar content) <- return $ lookup "content" m
        Just (Scalar time) <- return $ lookup "time" m
        time' <- return $ read time
        return $ Comment (T.pack name) (Textarea $ T.pack content) time'
#else
    -- this didn't work
    -- d <- doesFileExist commentsYaml
    -- unless d (saveComments [])
    return []
#endif

loadCommentsDat :: IO Comments
loadCommentsDat = do
    d <- doesFileExist commentsFile
    if d
        then do
            c <- S.readFile commentsFile
            case decode c of
                Left e -> error e
                Right x -> return x
        else return []

saveComments :: Comments -> IO ()
saveComments = writeFileL commentsYaml . L.fromChunks . return . Y.encode . commentsToSO

instance Serialize Text where
    put = put . encodeUtf8
    get = fmap (decodeUtf8With lenientDecode) get

instance Serialize Comment where
    put (Comment a b c) = put a >> put b >> put c
    get = Comment <$> get <*> get <*> get

instance Serialize Textarea where
    put (Textarea a) = put a
    get = fmap Textarea get

instance Serialize UTCTime where
    put = put . show
    get = fmap read get -- FIXME evil
