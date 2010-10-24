{-# LANGUAGE TypeFamilies, QuasiQuotes, OverloadedStrings #-}
import Yesod
data HelloWorld = HelloWorld
mkYesod "HelloWorld" [$parseRoutes|
/ HomeR GET
|]
instance Yesod HelloWorld where approot _ = ""
-- START
footer = do
    addStyle [$cassius|
#footer
    margin-top: 10px
    padding-top: 10px
    border-top: 1px dashed #000
    text-align: center
|]
    addBody [$hamlet|
#footer
    This page powered by the $
    %a!href="http://docs.yesodweb.com/" Yesod Web Framework
|]

getHomeR = defaultLayout $ do
    setTitle "Hello World"
    addStyle [$cassius|
p
    color: red
|]
    addBody [$hamlet|%p Hello World!|]
    footer
-- STOP
main = basicHandler 3000 HelloWorld
