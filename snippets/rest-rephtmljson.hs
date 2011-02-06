{-# LANGUAGE QuasiQuotes, TypeFamilies, OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
import Yesod
data R = R
mkYesod "R" [$parseRoutes|
/ RootR GET
/#String NameR GET
|]
instance Yesod R where approot _ = ""
-- START
getRootR = defaultLayout $ do
    setTitle "Homepage"
    addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/1.4/jquery.min.js"
    addJulius [$julius|
$(function(){
    $("a").click(function(){
        jQuery.getJSON($(this).attr("href"), function(o){
            $("div").text(o.name);
        });
        return false;
    });
});
|]
    let names = words "Larry Moe Curly"
    addHamlet [$hamlet|\
<div id="results">
    \Your results will be placed here if you have Javascript enabled.
<ul>
    $forall name <- names
        <li>
            <a href="@{NameR name}">#{name}
|]
getNameR name = do
    let widget = do
            setTitle $ string name
            addHamlet [$hamlet|\Looks like you have Javascript off. Name: #{name}
|]
    let json = jsonMap [("name", jsonScalar name)]
    defaultLayoutJson widget json
-- STOP
main = warpDebug 4000 R
