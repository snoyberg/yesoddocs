-- START
{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses #-}
import Yesod
data HelloWorld = HelloWorld
mkYesod "HelloWorld" [$parseRoutes|
/ HomeR GET
|]
instance Yesod HelloWorld where approot _ = ""
getHomeR = defaultLayout [$hamlet|\Hello World!
|]
main = warpDebug 3000 HelloWorld
-- STOP
