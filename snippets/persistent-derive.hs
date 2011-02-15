-- START
{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving #-}
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import Data.Time

data Employment = Employed | Unemployed | Retired
    deriving (Show, Read, Eq)
derivePersistField "Employment"

share [mkPersist, mkMigrate "migrateAll"] [$persist|
Person
    name String
    employment Employment
|]

main = withSqliteConn ":memory:" $ runSqlConn $ do
    runMigration migrateAll

    insert $ Person "Bruce Wayne" Retired
    insert $ Person "Peter Parker" Unemployed
    insert $ Person "Michael" Employed
-- STOP
