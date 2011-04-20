{-# LANGUAGE TypeFamilies, QuasiQuotes, StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses, TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
import Yesod
import Data.Text (Text)
data Wiki = Wiki
-- START
newtype WikiPage = WikiPage { unWikiPage :: [Text] }
instance MultiPiece WikiPage where
    toMultiPiece = unWikiPage
    -- Only rule is we can't have an empty name.
    -- Yesod itself ensures there are not empty strings within the list.
    fromMultiPiece [] = Nothing
    fromMultiPiece s = Just $ WikiPage s
mkYesod "Wiki" [$parseRoutes|
/wiki/*WikiPage    WikiR     GET
|]
-- STOP
deriving instance Read WikiPage
deriving instance Eq WikiPage
deriving instance Show WikiPage
instance Yesod Wiki where approot _ = ""
getWikiR (WikiPage s) = return $ RepPlain $ toContent $ show s
main = warpDebug 3000 Wiki
