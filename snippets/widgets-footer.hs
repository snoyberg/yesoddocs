{-# LANGUAGE TypeFamilies, QuasiQuotes, OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
import Yesod
data HelloWorld = HelloWorld
mkYesod "HelloWorld" [$parseRoutes|
/ HomeR GET
|]
instance Yesod HelloWorld where approot _ = ""
-- START
footer = do
    addCassius [$cassius|#footer
    margin-top: 10px
    padding-top: 10px
    border-top: 1px dashed #000
    text-align: center
|]
    -- Just keeping you on your toes: this could be addWidget or addHtml as ell.
    addHamlet [$hamlet|\
<div id="footer">
    \This page powered by the 
    <a href="http://docs.yesodweb.com/">Yesod Web Framework
|]

getHomeR = defaultLayout $ do
    setTitle "Hello World"
    addCassius [$cassius|p
    color: red
|]
    addHtml [$hamlet|<p>Hello World!
|]
    footer
-- STOP
main = warpDebug 3000 HelloWorld
