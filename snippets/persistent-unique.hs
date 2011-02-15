-- START
{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving #-}
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import Data.Time
import Control.Monad.IO.Class (liftIO)

share [mkPersist, mkMigrate "migrateAll"] [$persist|
Person
    firstName String
    lastName String
    age Int Gt Desc
    PersonName firstName lastName
|]

main = withSqliteConn ":memory:" $ runSqlConn $ do
    runMigration migrateAll
    insert $ Person "Michael" "Snoyman" 26
    michael <- getBy $ PersonName "Michael" "Snoyman"
    liftIO $ print michael
-- STOP
