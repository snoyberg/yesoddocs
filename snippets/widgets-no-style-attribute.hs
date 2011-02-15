{-# LANGUAGE TypeFamilies, QuasiQuotes, OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
    [$hamlet|<p>Hello World!|]
-- STOP
main = warpDebug 3000 HelloWorld
