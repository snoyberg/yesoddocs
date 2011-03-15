{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses, TemplateHaskell #-}
import Yesod
data HelloWorld = HelloWorld
mkYesod "HelloWorld" [$parseRoutes|/ HomeR GET|]
instance Yesod HelloWorld where approot _ = ""
getHomeR = defaultLayout $ do
    addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.5.0/jquery.min.js"
    [$hamlet|
<h1>Hello World!
<p>Here are some of my favorite links:
<ul>
    <li>
        <a href=http://docs.yesodweb.com/>Yesod Web Framework Docs
    <li>
        <a href=http://www.haskell.org/>Haskell Homepage
<p>Thanks for visiting!
|]
    addCassius
        [$cassius|
h1
    color: green
ul > li:first-child
    border-left: 5px solid orange
|]
    addJulius
-- START
        [$julius|
$(function(){
    $("h1").after("<p><a href='#' id='mylink'>Never click me</a></p>");
    $("#mylink").click(function(){
        alert("You clicked me!!! How dare you!");
        return false;
    });
});
|]
-- STOP
main = warpDebug 3000 HelloWorld
