-- START
{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving, TemplateHaskell, OverloadedStrings #-}
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH

share [mkPersist, mkMigrate "migrateAll"] [$persist|
Person
    name String
    age Int
Car
    color String
    make String
    model String
|]

main = withSqliteConn ":memory:" $ runSqlConn $ do
    runMigration migrateAll
-- STOP
