{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.UploadBlogs
    ( postUploadBlogsR
    ) where

import Wiki
import Entry
import Codec.Archive.Zip
import Data.Time (UTCTime (..), toGregorian)
import Data.Text (pack)
import Control.Monad (unless)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L

postUploadBlogsR :: Handler ()
postUploadBlogsR = do
    (uid, u) <- requireAuth
    unless (userAdmin u) $ permissionDenied ""
    (_, files) <- runRequestBody
    file <- maybe notFound (return . fileContent) $ lookup "zip" files
    runDB $ flip mapM_ (zEntries $ toArchive file) $ \entry -> do
        let fp = eRelativePath entry
        let toSlash '\\' = '/'
            toSlash c = c
        let slug = reverse $ takeWhile (/= '/') $ reverse $ map toSlash fp
        let contents = fromEntry entry
        post <- liftIO $ loadEntry fp $ S.concat $ L.toChunks contents
        let utc = UTCTime (entryDay post) 0
        fam <- insert $ TFamily utc
        topic <- insert $ Topic uid (entryTitle post) utc fam False
        _ <- insert $ TopicContent topic uid Nothing utc (entryFormat post) (entryContent post)
        tmap <- insert $ TMap uid (entryTitle post) utc
        _ <- insert $ TMapNode tmap Nothing 1 (Just topic) Nothing Nothing (MapNodeSlug $ pack slug)
        let (year, month, _) = toGregorian $ entryDay post
        _ <- insert $ Blog utc tmap (BlogSlug $ pack slug) (fromInteger year) month
        return ()
    setMessageI MsgBlogZipUploaded
    redirect RedirectTemporary BlogR
