-- START
{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving, TemplateHaskell, OverloadedStrings #-}
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import Data.Time

share [mkPersist, mkMigrate "migrateAll"] [$persist|
Person
    name String
    age Int
    color String Maybe
    created UTCTime default=now()
    language String default='Haskell'
    country String "default='El Salvador'"
|]

main = withSqliteConn ":memory:" $ runSqlConn $ do
    runMigration migrateAll
-- STOP
