{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Handler.Root
    ( getRootR
    , getPageR
    , getStaticContentR
    ) where

import Wiki
import Handler.Topic (getTopicR')
import Text.Hamlet (hamlet)
import Util (renderContent)

getRootR :: Handler RepHtml
getRootR = do
    mpage <- runDB $ getBy $ UniquePage ""
    mcontent <-
        case mpage of
            Nothing -> return Nothing
            Just (_, Page { pageTopic = tid }) -> runDB $ do
                x <- selectList [TopicContentTopic ==. tid] [Desc TopicContentChanged, LimitTo 1]
                case x of
                    [] -> return Nothing
                    (_, TopicContent {..}):_ -> return $ Just (topicContentFormat, topicContentContent, tid)
    let html' =
            case mcontent of
                Nothing -> [hamlet|
<h1>No homepage set.
<p>The site admin has no yet set a homepage topic.
|]
                Just (format, content, tid) -> renderContent tid format content
    defaultLayout $ addHamlet html'

getPageR :: Text -> Handler RepHtml
getPageR t = do
    p <- runDB $ getBy404 (UniquePage t)
    getTopicR' (return ()) False $ pageTopic $ snd p

getStaticContentR :: StaticContentId -> Handler StaticContent
getStaticContentR = runDB . get404
