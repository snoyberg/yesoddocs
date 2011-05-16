{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.Root
    ( getRootR
    , getPageR
    ) where

import Wiki
import Handler.Topic (getTopicR)

getRootR :: Handler RepHtml
getRootR = getPageR ""

getPageR :: Text -> Handler RepHtml
getPageR t = do
    p <- runDB $ getBy404 (UniquePage t)
    getTopicR $ pageTopic $ snd p
