{-# LANGUAGE TypeFamilies, QuasiQuotes, OverloadedStrings #-}
import Yesod
data OnRequest = OnRequest
mkYesod "OnRequest" [$parseRoutes|/ RootR GET|]
-- START
instance Yesod OnRequest where
    approot _ = ""
    onRequest = liftIO $ putStrLn "Hey, I got a request"
-- STOP
getRootR = defaultOnRequest [$hamlet|Hello World|]
main = basicHandler 4000 OnRequest
