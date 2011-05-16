{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Handler.Topic
    ( getTopicR
    , postTopicR
    ) where

import Wiki
import Util (renderContent, validateContent)
import Data.Text (pack)
import Control.Monad (unless)
import Text.Hamlet.NonPoly (html)

topicForm :: (Text, TopicFormat, Textarea, Maybe Text)
          -> Handler ((FormResult (Text, TopicFormat, Textarea, Maybe Text), Widget ()), Enctype)
topicForm (a, b, c, d) = runFormPost $ renderTable $ (,,,)
    <$> areq textField (fromLabel MsgTitle) (Just a) -- TRANS
    <*> areq (selectField formats) (fromLabel MsgFormat) (Just b)
    <*> areq textareaField (fromLabel MsgContent) (Just c)
    <*> aopt textField (fromLabel MsgSummary) (Just d)

getTopicR :: TopicId -> Handler RepHtml
getTopicR tid = do
    Topic {..} <- runDB $ get404 tid
    TopicContent {..} <- runDB $ do
        x <- selectList [TopicContentTopicEq tid] [TopicContentChangedDesc] 1 0
        case x of
            [y] -> return $ snd y
            [] -> lift notFound
            _ -> lift $ do
                $(logError) "Should only have returned 0 or 1 results"
                notFound
    owner <- runDB $ get404 topicOwner
    mauthor <- if topicOwner == topicContentAuthor then return Nothing else fmap Just $ runDB $ get404 topicContentAuthor
    ma <- maybeAuth
    let maid = fmap fst ma
        muser = fmap snd ma
    $(logDebug) $ pack $ concat ["maid: ", show maid, ", topicOwner", show topicOwner]
    mform <-
        if maid == Just topicOwner || fmap userAdmin muser == Just True
            then Just `fmap` (do
                ((_, w), e) <- topicForm (topicTitle, topicContentFormat, (Textarea $ topicContentContent), Nothing)
                return (w, e)
                )
            else return Nothing
    defaultLayout $(widgetFile "topic")

postTopicR :: TopicId -> Handler RepHtml
postTopicR tid = do
    Topic {..} <- runDB $ get404 tid
    TopicContent {..} <- runDB $ do
        x <- selectList [TopicContentTopicEq tid] [TopicContentChangedDesc] 1 0
        case x of
            [y] -> return $ snd y
            [] -> lift notFound
            _ -> lift $ do
                $(logError) "Should only have returned 0 or 1 results"
                notFound
    (aid, user) <- requireAuth
    unless (aid == topicOwner || userAdmin user) $ permissionDenied ""
    ((res, wform), enctype) <- topicForm (topicTitle, topicContentFormat, (Textarea $ topicContentContent), Nothing)
    case res of
        FormSuccess (title, format, (Textarea content), msummary) -> do
            now <- liftIO getCurrentTime
            _ <- runDB $ do
                update tid [TopicTitle title]
                _ <- insert $ TopicContent tid aid msummary now format $ validateContent format content
                addNewsItem ("Topic updated: " `mappend` title) (TopicR tid) [html|
<p>#{userName user} updated the topic: #{title}
$maybe summary <- msummary
    <p>Update summary: #{summary}
|]
                return ()
            setMessageI MsgTopicUpdated
            redirect RedirectTemporary $ TopicR tid
        _ -> defaultLayout $(widgetFile "topic_post")
