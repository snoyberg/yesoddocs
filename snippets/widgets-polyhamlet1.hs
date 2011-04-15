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
    setTitle "Polymorphic Hamlet"
    addHtml [$hamlet|<p>I was added with addHtml|]
    addHamlet [$hamlet|<p>I was added with addHamlet|]
    addWidget [$hamlet|<p>I was added with addWidget|]
-- STOP
main = warpDebug 3000 HelloWorld
