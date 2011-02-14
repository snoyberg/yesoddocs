-- START
{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving #-}
import Database.Persist
import Database.Persist.Sqlite
import Data.Time

share [mkPersist, mkMigrate "migrateAll"] [$persist|
Person
    name String
Car
    owner PersonId Eq
    name String
|]

main = withSqliteConn ":memory:" $ flip runSqlConn $ do
    runMigration migrateAll
    bruce <- insert $ Person "Bruce Wayne"
    insert $ Car bruce "Bat Mobile"
    insert $ Car bruce "Porsche"
    -- this could go on a while
    cars <- selectList [CarOwnerEq bruce] [] 0 0
    liftIO $ print cars
-- STOP
