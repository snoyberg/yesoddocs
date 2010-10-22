-- START
{-# LANGUAGE TypeFamilies, QuasiQuotes #-}
import Yesod
data HelloWorld = HelloWorld
mkYesod "HelloWorld" [$parseRoutes|
/ HomeR GET
|]
instance Yesod HelloWorld where approot _ = ""
getHomeR = defaultLayout [$hamlet|Hello World!|]
main = basicHandler 3000 HelloWorld
-- STOP
