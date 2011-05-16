{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.Settings
    ( getSettingsR
    , postSettingsR
    , postEditPageR
    ) where

import Wiki
import Control.Monad (unless)
import Data.Maybe (fromMaybe)

form' :: Text -> Handler ((FormResult Text, Widget ()), Enctype)
form' = runFormPost . renderTable . areq textField (FieldSettings MsgYourName Nothing Nothing Nothing) . Just

pageForm :: [(TopicId, Topic)] -> Maybe Page -> Handler ((FormResult (Maybe Text, TopicId), Widget ()), Enctype)
pageForm topics mp = runFormPost $ renderTable $ (,)
    <$> aopt textField (FieldSettings MsgPageName Nothing Nothing Nothing) (Just $ fmap pageName mp)
    <*> areq (selectField ts) (FieldSettings MsgPageTopic Nothing Nothing Nothing) (fmap pageTopic mp)
  where
    ts = map (\(tid, t) -> (topicTitle t, tid)) topics

getSettingsR :: Handler RepHtml
getSettingsR = do
    (uid, user) <- requireAuth
    ((_, form), enctype) <- form' $ userName user
    maps <- runDB $ selectList [TMapOwnerEq uid] [] 0 0
    topics <- runDB $ selectList [TopicOwnerEq uid] [] 0 0
    pages <- runDB $ selectList [] [] 0 0
    ((_, pageNew), _) <- pageForm topics Nothing
    pforms <- (fmap . fmap) (snd . fst) $ mapM (pageForm topics . Just . snd) pages
    defaultLayout $(widgetFile "settings")

postSettingsR :: Handler ()
postSettingsR = do
    uid <- requireAuthId
    ((res, _), _) <- form' ""
    case res of
        FormSuccess name -> do
            runDB $ update uid [UserName name]
            setMessageI MsgSettingsUpdated
        _ -> return ()
    redirect RedirectTemporary SettingsR

postEditPageR :: Handler ()
postEditPageR = do
    (uid, user) <- requireAuth
    unless (userAdmin user) $ permissionDenied ""
    topics <- runDB $ selectList [TopicOwnerEq uid] [] 0 0
    ((res, _), _) <- pageForm topics Nothing
    case res of
        FormSuccess (mname, tid) -> runDB $ do
            let x = Page (fromMaybe "" mname) tid
            deleteBy $ UniquePage $ pageName x
            _ <- insert x
            return ()
        _ -> return ()
    redirect RedirectTemporary SettingsR
