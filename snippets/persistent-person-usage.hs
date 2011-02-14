{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving #-}
import Database.Persist
import Database.Persist.Sqlite

mkPersist [$persist|
Person
    name String
    age Int
|]
-- START
main = withSqliteConn ":memory:" $ flip runSqlConn $ do
    michaelId <- insert $ Person "Michael" 26
    michael <- get michaelId
    liftIO $ print michael
-- STOP
