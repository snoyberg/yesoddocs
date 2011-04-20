-- START
{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving, TemplateHaskell, OverloadedStrings #-}
import Database.Persist
import Database.Persist.TH
import Database.Persist.Sqlite

mkPersist [$persist|
Person
    name String
    age Int
|]
-- STOP
main = return ()
