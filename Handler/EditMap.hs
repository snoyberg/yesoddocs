{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.EditMap
    ( getEditMapR
    , postEditMapR
    ) where

import Wiki

getEditMapR :: TMapId -> Handler RepHtml
getEditMapR = undefined

postEditMapR :: TMapId -> Handler RepHtml
postEditMapR = undefined
