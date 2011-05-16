{-# LANGUAGE TypeFamilies, QuasiQuotes, TemplateHaskell, OverloadedStrings, MultiParamTypeClasses #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Controller
    ( withYesodDocs
    ) where

import Yesod
import Yesod.Helpers.Static
import Entry
import Book
import Control.Concurrent.AdvSTM.TVar
import Comments
import YesodDocs
import qualified Data.Object.Yaml as Y
import Data.Object
import Control.Monad (join)

import Handler.Home
import Handler.Blog
import Handler.Book

mkYesodDispatch "YesodDocs" resourcesYesodDocs

withYesodDocs :: (Application -> IO a) -> IO a
withYesodDocs f = do
    entries <- loadEntries
    let s = static "static"
    book <- loadBook
    cs <- loadComments
    tcomments <- newTVarIO cs
    conts <- loadConts
    app <- toWaiApp $ YesodDocs s entries book tcomments conts
    f app

loadConts :: IO [Contributor]
loadConts = do
    Sequence o <- join $ Y.decodeFile "contributors.yaml"
    mapM go (o :: [TextObject])
  where
    go (Mapping m) = do
        Just (Scalar name) <- return $ lookup "name" m
        Just (Scalar email) <- return $ lookup "email" m
        Just (Scalar url) <- return $ lookup "url" m
        Just (Scalar bio) <- return $ lookup "bio" m
        return $ Contributor name email url bio
    go _ = fail "Invalid loadConts"
