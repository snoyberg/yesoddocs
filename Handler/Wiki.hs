{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Handler.Wiki
    ( getWikiR
    , postWikiR
    , postUnlinkWikiR
    ) where

import Wiki
import qualified Data.Text as T
import Handler.Topic (getTopicR')

wikiForm :: Handler ((FormResult TopicId, Widget), Enctype)
wikiForm = do
    ts <- runDB $ selectList [TopicAllWrite ==. True] [Asc TopicTitle]
    runFormPost $ renderTable $ areq (selectField $ map go ts) (FieldSettings MsgWikiTopic Nothing Nothing Nothing) Nothing
  where
    go :: (TopicId, Topic) -> (Text, TopicId)
    go (tid, t) = (topicTitle t, tid)

getWikiR :: [Text] -> Handler RepHtml
getWikiR ps = do
    x <- runDB $ getBy $ UniqueWikiPage $ T.intercalate "/" ps
    muid <- maybeAuthId
    ((_, form), _) <- wikiForm
    case x of
        Nothing -> defaultLayout $(widgetFile "wiki-blank")
        Just (_, wp) -> getTopicR' $(widgetFile "wiki-unlink") False $ wikiPageTopic wp

postWikiR :: [Text] -> Handler RepHtml
postWikiR ps = do
    uid <- requireAuthId
    ((res, form), _) <- wikiForm
    case res of
        FormSuccess tid -> do
            runDB $ do
                let p = T.intercalate "/" ps
                deleteBy $ UniqueWikiPage p
                _ <- insert $ WikiPage p tid
                return ()
            redirect RedirectTemporary $ WikiR ps
        _ -> do
            let muid = Just uid
            defaultLayout $(widgetFile "wiki-blank")

postUnlinkWikiR :: [Text] -> Handler ()
postUnlinkWikiR ps = do
    _ <- requireAuthId
    runDB $ deleteBy $ UniqueWikiPage $ T.intercalate "/" ps
    redirect RedirectTemporary $ WikiR ps
