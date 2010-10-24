{-# LANGUAGE TypeFamilies, QuasiQuotes, OverloadedStrings #-}
import Yesod
data HelloWorld = HelloWorld
mkYesod "HelloWorld" [$parseRoutes|
/ HomeR GET
|]
instance Yesod HelloWorld where approot _ = ""
-- START
colorChanger = do
    addStyle [$cassius|
#color-text
    color: red
#color-text.green
    color: green
|]
    addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/1.4/jquery.min.js"
    addJavascript [$julius|
$(function(){
    $("#color-text").click(function(){
        $("#color-text").toggleClass("green");
    });
});
|]
    addBody [$hamlet|#color-text This text changes colors. Just click on it!|]

buttons = do
    addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/1.4/jquery.min.js"
    addScriptRemote "http://ajax.googleapis.com/ajax/libs/jqueryui/1.8/jquery-ui.min.js"
    addStylesheetRemote "http://ajax.googleapis.com/ajax/libs/jqueryui/1.8/themes/ui-lightness/jquery-ui.css"
    addJavascript [$julius|
$(function(){
    $("button").button().click(function(){
        alert("You clicked the button.");
    });
});
|]
    addBody [$hamlet|
%button This is a button. I dare you to click it.
|]

getHomeR = defaultLayout $ do
    setTitle "Hello World"
    colorChanger
    buttons
-- STOP
main = basicHandler 3000 HelloWorld
