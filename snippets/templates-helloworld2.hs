{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses #-}
import Yesod
data HelloWorld = HelloWorld
mkYesod "HelloWorld" [$parseRoutes|/ HomeR GET|]
instance Yesod HelloWorld where approot _ = ""
-- START
getHomeR = hamletToRepHtml [$hamlet|\
<h1>Hello World!
<p>Here are some of my favorite links:
<ul>
    <li>
        <a href="http://docs.yesodweb.com/">Yesod Web Framework Docs
    <li>
        <a href="http://www.haskell.org/">Haskell Homepage
<p>Thanks for visiting!
|]
-- STOP
main = warpDebug 3000 HelloWorld
