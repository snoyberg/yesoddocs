-- START
{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving #-}
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import Data.Time

share [mkPersist, mkMigrate "migrateAll"] [$persist|
Person
    name String
Store
    name String
PersonStore
    person PersonId
    store StoreId
    UniquePersonStore person store
|]

main = withSqliteConn ":memory:" $ runSqlConn $ do
    runMigration migrateAll

    bruce <- insert $ Person "Bruce Wayne"
    michael <- insert $ Person "Michael"

    target <- insert $ Store "Target"
    gucci <- insert $ Store "Gucci"
    sevenEleven <- insert $ Store "7-11"

    insert $ PersonStore bruce gucci
    insert $ PersonStore bruce sevenEleven

    insert $ PersonStore michael target
    insert $ PersonStore michael sevenEleven
-- STOP
