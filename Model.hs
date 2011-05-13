{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving, TemplateHaskell, OverloadedStrings #-}
module Model where

import Yesod.Persist
import Data.Text (Text)
import Data.Time (UTCTime)
import Text.Hamlet (Html)

data TopicFormat = TFHtml | TFMarkdown | TFText | TFDita
    deriving (Read, Eq, Show)
derivePersistField "TopicFormat"

formats :: [(Text, TopicFormat)]
formats =
    [ ("HTML", TFHtml)
    , ("Markdown", TFMarkdown)
    , ("Plain text", TFText)
    , ("DITA", TFDita)
    ]

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist, mkMigrate "migrateAll"] $(persistFile "config/models")
