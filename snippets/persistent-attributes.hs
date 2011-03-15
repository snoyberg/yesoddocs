-- START
{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving, TemplateHaskell #-}
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
|]

main = withSqliteConn ":memory:" $ runSqlConn $ do
    runMigration migrateAll
-- STOP
