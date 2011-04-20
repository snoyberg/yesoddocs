-- START
{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses, TemplateHaskell, OverloadedStrings #-}
import Yesod

data Links = Links

mkYesod "Links" [$parseRoutes|
/ HomeR GET
/page1 Page1R GET
/page2 Page2R GET
|]

instance Yesod Links where
    approot _ = ""

getHomeR  = defaultLayout [$hamlet|<a href="@{Page1R}">Go to page 1!|]
getPage1R = defaultLayout [$hamlet|<a href="@{Page2R}">Go to page 2!|]
getPage2R = defaultLayout [$hamlet|<a href="@{HomeR}">Go home!|]

main = warpDebug 3000 Links
-- STOP
