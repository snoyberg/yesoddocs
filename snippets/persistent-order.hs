-- START
{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving #-}
import Database.Persist
import Database.Persist.Sqlite
import Data.Time

share [mkPersist, mkMigrate "migrateAll"] [$persist|
Person
    name String Eq
    age Int Gt Desc
|]

main = withSqliteConn ":memory:" $ flip runSqlConn $ do
    runMigration migrateAll
    michaels <- select
        [ PersonNameEq "Michael"
        , PersonAge Gt 25
        ]
        [ PersonAgeDesc
        ] 0 0 -- we will explain these later, all in good time
    liftIO $ print michaels
-- STOP
