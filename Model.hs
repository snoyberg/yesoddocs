{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving, TemplateHaskell, OverloadedStrings, TypeSynonymInstances #-}
module Model where

import Yesod.Persist
import Database.Persist.Base (DeleteCascade (..))
import Yesod.Content (HasReps (..), toContent)
import Data.Text (Text, pack)
import Data.Time (UTCTime)
import Text.Hamlet (Html)
import Yesod.Form (Textarea)
import Yesod.Core (SinglePiece (..), MultiPiece (..))
import Data.ByteString (ByteString)
import Data.ByteString.Base64 (decodeLenient)

data TopicFormat = TFMarkdown | TFHtml | TFText | TFDitaConcept | TFDitaTopic
    deriving (Read, Eq, Show)
derivePersistField "TopicFormat"

newtype MapNodeSlug = MapNodeSlug { unMapNodeSlug :: Text }
    deriving (Read, Eq, Show, PersistField, SinglePiece, Ord)
type MapNodeSlugs = [MapNodeSlug]
instance MultiPiece MapNodeSlugs where
    toMultiPiece = map unMapNodeSlug
    fromMultiPiece = fmap (map MapNodeSlug) . fromMultiPiece
newtype BlogSlug = BlogSlug Text
    deriving (Read, Eq, Show, PersistField, SinglePiece, Ord)
newtype UserHandle = UserHandle { unUserHandle :: Text }
    deriving (Read, Eq, Show, PersistField, SinglePiece, Ord)

newtype Month = Month Int
    deriving (Read, Eq, Show, PersistField, Ord)
instance SinglePiece Month where
    toSinglePiece (Month i)
        | i < 10 && i >= 0 = pack $ '0' : show i
        | otherwise = toSinglePiece i
    fromSinglePiece t = do
        i <- fromSinglePiece t
        if i >= 1 && i <= 12
            then Just $ Month i
            else Nothing

formats :: [(Text, TopicFormat)]
formats =
    [ ("Markdown", TFMarkdown)
    , ("HTML", TFHtml)
    , ("Plain text", TFText)
    , ("DITA Concept", TFDitaConcept)
    , ("DITA Topic", TFDitaTopic)
    ]

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist, mkMigrate "migrateAll", mkDeleteCascade] $(persistFile "config/models")

instance HasReps StaticContent where
    chooseRep (StaticContent mt content) _ = return (mt, toContent $ decodeLenient content)
