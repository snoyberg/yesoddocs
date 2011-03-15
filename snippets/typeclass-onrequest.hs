{-# LANGUAGE TypeFamilies, QuasiQuotes, OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses, TemplateHaskell #-}
import Yesod
data OnRequest = OnRequest
mkYesod "OnRequest" [$parseRoutes|/ RootR GET|]
-- START
instance Yesod OnRequest where
    approot _ = ""
    {- FIXME this is no longer in Yesod
    onRequest = liftIO $ putStrLn "Hey, I got a request"
    -}
-- STOP
getRootR = defaultLayout [$hamlet|\Hello World
|]
main = warpDebug 4000 OnRequest
