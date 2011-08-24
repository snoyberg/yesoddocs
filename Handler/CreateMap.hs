{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.CreateMap
    ( getCreateMapR
    , postCreateMapR
    ) where

import Wiki

mapForm :: Handler ((FormResult Text, Widget), Enctype)
mapForm = runFormPost $ renderTable $ id
    <$> areq textField (FieldSettings MsgTitle (Just MsgMapTitleTooltip) Nothing Nothing) Nothing

getCreateMapR :: Handler RepHtml
getCreateMapR = do
    _ <- requireAuthId
    ((_, form), enctype) <- mapForm
    defaultLayout $(widgetFile "create-map")

postCreateMapR :: Handler RepHtml
postCreateMapR = do
    aid <- requireAuthId
    ((res, form), enctype) <- mapForm
    case res of
        FormSuccess title -> do
            now <- liftIO getCurrentTime
            m <- runDB $ insert $ TMap aid title now
            setMessageI $ MsgMapCreated title
            redirect RedirectTemporary $ EditMapR m
        _ -> return ()
    defaultLayout $(widgetFile "create-map")
