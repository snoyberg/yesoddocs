-- START
{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving #-}
import Database.Persist
import Database.Persist.Sqlite
import Data.Time

share [mkPersist, mkMigrate "migrateAll"] [$persist|
Person
    name String Eq Update
    age Int Gt
|]

main = withSqliteConn ":memory:" $ flip runSqlConn $ do
    runMigration migrateAll
    updateWhere
        [ PersonNameEq "Michael"
        , PersonAge Gt 25
        ]
        [ PersonName "Mike"
        ]
-- STOP
