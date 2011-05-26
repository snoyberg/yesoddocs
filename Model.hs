{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving, TemplateHaskell, OverloadedStrings #-}
module Model where

import Yesod.Persist
import Yesod.Content (HasReps (..), toContent)
import Data.Text (Text)
import Data.Time (UTCTime)
import Text.Hamlet (Html)
import Yesod.Form (Textarea)
import Yesod.Core (SinglePiece)
import Data.ByteString (ByteString)

data TopicFormat = TFHtml | TFMarkdown | TFText | TFDitaConcept | TFDitaTopic
    deriving (Read, Eq, Show)
derivePersistField "TopicFormat"

newtype MapNodeSlug = MapNodeSlug Text
    deriving (Read, Eq, Show, PersistField, SinglePiece, Ord)
newtype BlogSlug = BlogSlug Text
    deriving (Read, Eq, Show, PersistField, SinglePiece, Ord)
newtype UserHandle = UserHandle { unUserHandle :: Text }
    deriving (Read, Eq, Show, PersistField, SinglePiece, Ord)

formats :: [(Text, TopicFormat)]
formats =
    [ ("HTML", TFHtml)
    , ("Markdown", TFMarkdown)
    , ("Plain text", TFText)
    , ("DITA Concept", TFDitaConcept)
    , ("DITA Topic", TFDitaTopic)
    ]

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist, mkMigrate "migrateAll"] $(persistFile "config/models")

instance HasReps StaticContent where
    chooseRep (StaticContent mt content) _ = return (mt, toContent content)
