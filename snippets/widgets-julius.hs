{-# LANGUAGE TypeFamilies, QuasiQuotes, OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses, TemplateHaskell #-}
import Yesod
data HelloWorld = HelloWorld
mkYesod "HelloWorld" [$parseRoutes|
/ HomeR GET
|]
instance Yesod HelloWorld where approot _ = ""
-- START
getHomeR = defaultLayout $ do
    setTitle "Hello World"
    addJulius [$julius|alert("This is Javascript");|]
    addWidget [$hamlet|<p>Hello World!
|]
-- STOP
main = warpDebug 3000 HelloWorld
