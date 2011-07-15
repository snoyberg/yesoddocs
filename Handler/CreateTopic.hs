{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.CreateTopic
    ( getCreateTopicR
    , postCreateTopicR
    , richEdit
    ) where

import Wiki
import Util (validateContent)
import Text.Hamlet.NonPoly (html)
import Handler.Search (updateTerms)

topicForm :: Handler ((FormResult (Text, TopicFormat, Textarea, Bool), Widget ()), Enctype)
topicForm = runFormPost $ renderTable $ (,,,)
    <$> areq textField (fromLabel MsgTitle) Nothing
    <*> areq (selectField formats) (FieldSettings MsgFormat Nothing (Just "format") Nothing) Nothing
    <*> areq textareaField (FieldSettings MsgContent Nothing (Just "contentarea") Nothing) Nothing
    <*> areq boolField (FieldSettings MsgWorldWriteable (Just MsgWorldWriteableTooltip) Nothing Nothing) (Just False)

richEdit :: Widget ()
richEdit = do
    addScript $ StaticR jquery_js
    addScript $ StaticR nicEdit_js
    addJulius $(juliusFile "rich-edit")

getCreateTopicR :: Handler RepHtml
getCreateTopicR = do
    _ <- requireAuthId
    ((_, form), enctype) <- topicForm
    let merr = Nothing :: Maybe Text
    defaultLayout $ richEdit >> $(widgetFile "create-topic")

postCreateTopicR :: Handler RepHtml
postCreateTopicR = do
    (aid, user) <- requireAuth
    ((res, form), enctype) <- topicForm
    case res of
        FormSuccess (title, format, (Textarea content), allWrite) -> do
            now <- liftIO getCurrentTime
            topic <- runDB $ do
                fam <- insert $ TFamily now
                topic <- insert $ Topic aid title now fam allWrite
                let tc = TopicContent topic aid Nothing now format $ validateContent format content
                _ <- insert tc
                updateTerms tc
                addNewsItem ("New topic created: " `mappend` title) (TopicR topic) Nothing [html|
<p>#{userName user} created a new topic: #{title}
|]
                return topic
            setMessageI $ MsgTopicCreated title
            redirect RedirectTemporary $ TopicR topic
        _ -> do
            let merr = Just MsgInvalidTopic
            defaultLayout $(widgetFile "create-topic")
