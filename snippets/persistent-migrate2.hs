-- START
{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving #-}
import Database.Persist
import Database.Persist.Sqlite

share [mkPersist, mkMigrate "migrateAll"] [$persist|
Person
    name String
    age Int
Car
    color String
    make String
    model String
|]

main = withSqliteConn ":memory:" $ flip runSqlConn $ do
    runMigration migrateAll
-- STOP
