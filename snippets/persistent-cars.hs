-- START
{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving, TemplateHaskell #-}
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import Control.Monad.IO.Class (liftIO)
import Data.Time

share [mkPersist, mkMigrate "migrateAll"] [$persist|
Person
    name String
Car
    owner PersonId Eq
    name String
|]

main = withSqliteConn ":memory:" $ runSqlConn $ do
    runMigration migrateAll
    bruce <- insert $ Person "Bruce Wayne"
    insert $ Car bruce "Bat Mobile"
    insert $ Car bruce "Porsche"
    -- this could go on a while
    cars <- selectList [CarOwnerEq bruce] [] 0 0
    liftIO $ print cars
-- STOP
