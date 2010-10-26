{-# LANGUAGE TypeFamilies, QuasiQuotes, OverloadedStrings #-}
import Yesod
data HelloWorld = HelloWorld
mkYesod "HelloWorld" [$parseRoutes|
/ HomeR GET
|]
instance Yesod HelloWorld where approot _ = ""
-- START
getHomeR = defaultLayout $ do
    setTitle "Hello World"
    addCassius [$cassius|
p
    color: red
|]
    addWidget [$hamlet|%p Hello World!|]
-- STOP
main = basicHandler 3000 HelloWorld
