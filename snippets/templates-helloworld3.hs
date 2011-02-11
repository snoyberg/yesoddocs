{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses #-}
import Yesod
data HelloWorld = HelloWorld
mkYesod "HelloWorld" [$parseRoutes|/ HomeR GET|]
instance Yesod HelloWorld where approot _ = ""
getHomeR = defaultLayout $ do
    [$hamlet|
<h1>Hello World!
<p>Here are some of my favorite links:
<ul>
    <li>
        <a href="http://docs.yesodweb.com/">Yesod Web Framework Docs
    <li>
        <a href="http://www.haskell.org/">Haskell Homepage
<p>Thanks for visiting!
|]
    addCassius
-- START
        [$cassius|
h1
    color: green
ul > li:first-child
    border-left: 5px solid orange
|]
-- STOP
main = warpDebug 3000 HelloWorld
