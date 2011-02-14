-- START
{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving #-}
import Database.Persist
import Database.Persist.Sqlite
import Data.Time

share [mkPersist, mkMigrate "migrateAll"] [$persist|
Person
    name String
    age Int
    color String Maybe
    created UTCTime default=now()
|]

main = withSqliteConn ":memory:" $ flip runSqlConn $ do
    runMigration migrateAll
-- STOP
