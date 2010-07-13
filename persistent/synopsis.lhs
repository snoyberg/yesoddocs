---
title: Synopsis - Persistent
---
This example uses the sqlite backend for Persistent, since it can run in-memory and has no external dependencies.

> {-# LANGUAGE TypeFamilies, GeneralizedNewtypeDeriving, QuasiQuotes #-}
>
> import Database.Persist.Sqlite
> import Control.Monad.IO.Class (liftIO)
>
> mkPersist [$persist|Person
>     name String Eq
>     age Int update
> |]
>
> main :: IO ()
> main = withSqlite ":memory:" 8 $ runSqlite go
>
> go :: SqliteReader IO ()
> go = do
>   initialize (undefined :: Person)
>   key <- insert $ Person "Michael" 25
>   liftIO $ print key
>   p1 <- get key
>   liftIO $ print p1
>   update key [PersonAge 26]
>   p2 <- get key
>   liftIO $ print p2
>   p3 <- select [PersonNameEq "Michael"] []
>   liftIO $ print p3
>   delete key
>   p4 <- select [PersonNameEq "Michael"] []
>   liftIO $ print p4

The output of the above is:

    PersonId 1
    Just (Person {personName = "Michael", personAge = 25})
    Just (Person {personName = "Michael", personAge = 26})
    [(PersonId 1,Person {personName = "Michael", personAge = 26})]
    []
