{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Handler.Settings
    ( getSettingsR
    , postSettingsR
    , postEditPageR
    , postAddBlogMapR
    ) where

import Wiki
import Util
import Control.Monad (unless)
import Data.Maybe (fromMaybe)
import Control.Applicative (pure)

form' :: User -> Handler ((FormResult User, Widget ()), Enctype)
form' User {..} = runFormPost $ renderTable $ User
    <$> pure userIdent
    <*> areq textField (FieldSettings MsgYourName Nothing Nothing Nothing) (Just userName)
    <*> pure userAdmin
    <*> areq textField (FieldSettings MsgYourHandle (Just MsgHandleTooltip) Nothing Nothing) (Just userHandle)
    <*> aopt emailField (FieldSettings MsgYourEmail (Just MsgEmailTooltip) Nothing Nothing) (Just userEmail)
    <*> aopt urlField (FieldSettings MsgYourUrl Nothing Nothing Nothing) (Just userUrl)
    <*> aopt textareaField (FieldSettings MsgYourBio Nothing Nothing Nothing) (Just userBio)

pageForm :: [(TopicId, Topic)] -> Maybe Page -> Handler ((FormResult (Maybe Text, TopicId), Widget ()), Enctype)
pageForm topics mp = runFormPost $ renderTable $ (,)
    <$> aopt textField (FieldSettings MsgPageName Nothing Nothing Nothing) (Just $ fmap pageName mp)
    <*> areq (selectField ts) (FieldSettings MsgPageTopic Nothing Nothing Nothing) (fmap pageTopic mp)
  where
    ts = map (\(tid, t) -> (topicTitle t, tid)) topics

getSettingsR :: Handler RepHtml
getSettingsR = do
    (uid, user) <- requireAuth
    ((_, form), enctype) <- form' user
    maps <- runDB $ selectList [TMapOwnerEq uid] [] 0 0
    topics <- runDB $ selectList [TopicOwnerEq uid] [] 0 0
    pages <- runDB $ selectList [] [] 0 0
    ((_, pageNew), _) <- pageForm topics Nothing
    pforms <- (fmap . fmap) (snd . fst) $ mapM (pageForm topics . Just . snd) pages
    ((_, ab), _) <- addBlog uid
    defaultLayout $(widgetFile "settings")

postSettingsR :: Handler ()
postSettingsR = do
    (uid, user) <- requireAuth
    ((res, _), _) <- form' user
    case res of
        FormSuccess user' -> do
            -- FIXME check for duplicate handle
            runDB $ replace uid user'
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

addBlog :: UserId -> Handler ((FormResult (TMapId, Text), Widget ()), Enctype)
addBlog uid = do
    maps <- runDB $ selectList [TMapOwnerEq uid] [] 0 0
    runFormPost $ renderTable $ (,)
        <$> areq (selectField $ map go maps) (FieldSettings MsgMapForBlog Nothing Nothing Nothing) Nothing
        <*> areq textField (FieldSettings MsgSlug (Just MsgSlugTooltip) Nothing Nothing) Nothing
  where
    go (x, y) = (tMapTitle y, x)

postAddBlogMapR :: Handler ()
postAddBlogMapR = do
    (uid, u) <- requireAuth
    ((res, _), _) <- addBlog uid
    case res of
        FormSuccess (tmid, slug) -> do
            -- FIXME check for unique slug
            now <- liftIO getCurrentTime
            _ <- runDB $ insert $ Blog uid now tmid slug
            redirect RedirectTemporary $ BlogPostR (userHandle u) slug
        _ -> do
            setMessageI MsgInvalidBlogInfo
            redirect RedirectTemporary SettingsR
