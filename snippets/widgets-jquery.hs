{-# LANGUAGE TypeFamilies, QuasiQuotes, OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
import Yesod
data HelloWorld = HelloWorld
mkYesod "HelloWorld" [$parseRoutes|
/ HomeR GET
|]
instance Yesod HelloWorld where approot _ = ""
-- START
colorChanger = do
    addCassius [$cassius|#color-text
    color: red
#color-text.green
    color: green
|]
    addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/1.4/jquery.min.js"
    addJulius [$julius|
$(function(){
    $("#color-text").click(function(){
        $("#color-text").toggleClass("green");
    });
});
|]
    addHamlet [$hamlet|<div id="color-text">This text changes colors. Just click on it!
|]

buttons = do
    addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/1.4/jquery.min.js"
    addScriptRemote "http://ajax.googleapis.com/ajax/libs/jqueryui/1.8/jquery-ui.min.js"
    addStylesheetRemote "http://ajax.googleapis.com/ajax/libs/jqueryui/1.8/themes/ui-lightness/jquery-ui.css"
    addJulius [$julius|
$(function(){
    $("button").button().click(function(){
        alert("You clicked the button.");
    });
});
|]
    addHamlet [$hamlet|\
<button>This is a button. I dare you to click it.
|]

getHomeR = defaultLayout $ do
    setTitle "Hello World"
    colorChanger
    buttons
-- STOP
main = warpDebug 3000 HelloWorld
