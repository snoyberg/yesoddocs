-- START
{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving #-}
import Database.Persist
import Database.Persist.Sqlite

mkPersist [$persist|
Person
    name String
    age Int
|]

main = withSqliteConn ":memory:" $ flip runSqlConn $ do
    runMigration $ migrate (undefined :: Person) -- this line added: that's it!
    michaelId <- insert $ Person "Michael" 26
    michael <- get michaelId
    liftIO $ print michael
-- STOP
