{-# LANGUAGE TypeFamilies, QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
import Yesod
data Simple = Simple
-- START
mkYesod "Simple" [$parseRoutes|
/ HomeR GET
|]
getHomeR :: GHandler subsite Simple RepHtml
getHomeR = defaultLayout [$hamlet|<h1>This is simple
|]
-- STOP
instance Yesod Simple where approot _ = ""
main = warpDebug 3000 Simple
