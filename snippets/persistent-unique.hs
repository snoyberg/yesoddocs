-- START
{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving #-}
import Database.Persist
import Database.Persist.Sqlite
import Data.Time

share [mkPersist, mkMigrate "migrateAll"] [$persist|
Person
    firstName String
    lastName String
    age Int Gt Desc
    PersonName firstName lastName
|]

main = withSqliteConn ":memory:" $ flip runSqlConn $ do
    runMigration migrateAll
    insert $ Person "Michael" "Snoyman" 26
    michael <- getBy $ PersonName "Michael" "Snoyman"
    liftIO $ print michael
-- STOP
