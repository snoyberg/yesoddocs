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
    app <- toWaiApp $ YesodDocs s entries book tcomments
    f app
