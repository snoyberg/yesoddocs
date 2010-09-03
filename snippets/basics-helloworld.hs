-- START
{-# LANGUAGE TypeFamilies, QuasiQuotes #-}
import Yesod
data HelloWorld = HelloWorld
mkYesod "HelloWorld" [$parseRoutes|/ HomeR GET|]
instance Yesod HelloWorld where approot _ = ""
getHomeR = return $ RepPlain $ toContent "Hello World!"
main = basicHandler 3000 HelloWorld
-- STOP
