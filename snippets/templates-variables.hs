{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses, TemplateHaskell #-}
import Yesod
data HelloWorld = HelloWorld
mkYesod "HelloWorld" [$parseRoutes|/ HomeR GET|]
instance Yesod HelloWorld where approot _ = ""

getHomeR = defaultLayout $ do
-- START
    let name = "Michael"
    let nameId = "name"

    addHamlet [$hamlet|
<h1>Hello World!
<p>
    Welcome to my system. Your name is #
    <span ##{nameId}>#{name}
    . Enjoy your stay!
<p
    <a href=@{HomeR}>Return Home
|]

    addCassius [$cassius|
##{nameId}
    color: green
|]

    addJulius [$julius|
alert("Welcome #{name}");
|]
-- STOP
main = warpDebug 3000 HelloWorld
