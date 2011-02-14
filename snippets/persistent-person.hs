-- START
{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving #-}
import Database.Persist
import Database.Persist.Sqlite

mkPersist [$persist|
Person
    name String
    age Int
|]
-- STOP
main = return ()
