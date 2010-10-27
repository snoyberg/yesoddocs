{-# LANGUAGE TypeFamilies, QuasiQuotes #-}
import Yesod
data Layout = Layout
mkYesod "Layout" [$parseRoutes|/ RootR GET|]
instance Yesod Layout where
    approot _ = ""
-- START
    defaultLayout contents = do
        PageContent title headTags bodyTags <- widgetToPageContent $ do
            addCassius [$cassius|
#body
    font-family: sans-serif
#wrapper
    width: 760px
    margin: 0 auto
|]
            addWidget contents
        hamletToRepHtml [$hamlet|
!!!
%html
    %head
        %title $title$
        ^headTags^
    %body
        #wrapper
            ^bodyTags^
|]
-- STOP
getRootR = defaultLayout $ do
    setTitle $ string "Root test"
    addCassius [$cassius|
body
    color: red
|]
    addHamlet [$hamlet|%h1 Hello|]
main = basicHandler 4000 Layout
