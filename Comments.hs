{-# OPTIONS_GHC -fno-warn-orphans #-}
module Comments where

import Yesod hiding (get)
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Encoding (encodeUtf8, decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Data.Time
import Data.Serialize
import System.IO.Cautious
import Prelude hiding (writeFile)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Control.Applicative
import System.Directory (doesFileExist)

data Comment = Comment
    { commentName :: Text
    , commentContent :: Textarea
    , commentTime :: UTCTime
    }

type Comments = [(String, [(Text, [Comment])])]

commentsFile :: String
commentsFile = "comments.dat"

loadComments :: IO Comments
loadComments = do
    d <- doesFileExist commentsFile
    if d
        then do
            c <- S.readFile commentsFile
            case decode c of
                Left e -> error e
                Right x -> return x
        else return []

saveComments :: Comments -> IO ()
saveComments = writeFileL commentsFile . L.fromChunks . return . encode

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
