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
    created UTCTime default=CURRENT_TIMESTAMP
    language String default='Haskell'
|]

main = withSqliteConn ":memory:" $ runSqlConn $ do
    runMigration migrateAll
-- STOP
