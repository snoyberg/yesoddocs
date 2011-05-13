{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.CreateTopic
    ( getCreateTopicR
    , postCreateTopicR
    ) where

import Wiki
import Control.Applicative
import Data.Text (Text)
import Util (validateContent)
import Text.Hamlet.NonPoly (html)

topicForm :: Handler ((FormResult (Text, TopicFormat, Textarea), Widget ()), Enctype)
topicForm = runFormPost $ renderTable $ (,,)
    <$> areq textField "Title" Nothing
    <*> areq (selectField formats) "Format" Nothing
    <*> areq textareaField "Content" Nothing

getCreateTopicR :: Handler RepHtml
getCreateTopicR = do
    _ <- requireAuthId
    ((_, form), enctype) <- topicForm
    let merr = Nothing
    defaultLayout $(widgetFile "create-topic")

postCreateTopicR :: Handler RepHtml
postCreateTopicR = do
    (aid, user) <- requireAuth
    ((res, form), enctype) <- topicForm
    case res of
        FormSuccess (title, format, (Textarea content)) -> do
            now <- liftIO getCurrentTime
            topic <- runDB $ do
                fam <- insert $ TFamily now
                topic <- insert $ Topic aid title now fam
                _ <- insert $ TopicContent topic aid Nothing now format $ validateContent format content
                addNewsItem ("New topic created: " `mappend` title) (TopicR topic) [html|
<p>#{userName user} created a new topic: #{title}
|]
                return topic
            setMessage $ MsgTopicCreated title
            redirect RedirectTemporary $ TopicR topic
        _ -> do
            let merr = Just MsgInvalidTopic
            defaultLayout $(widgetFile "create-topic")
