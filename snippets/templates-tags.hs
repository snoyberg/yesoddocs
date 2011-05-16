{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses, TemplateHaskell, OverloadedStrings #-}
import Yesod
data HelloWorld = HelloWorld
mkYesod "HelloWorld" [$parseRoutes|/ HomeR GET|]
instance Yesod HelloWorld where approot _ = ""

getHomeR = defaultLayout $ do
-- START
    [$hamlet|
<p #my-id .my-class
    This paragraph has an ID and a class. It also has #
    <b>bold
    \ and #
    <i>italic
    , and shows you how to control whitespace.
|]
-- STOP
main = warpDebug 3000 HelloWorld
