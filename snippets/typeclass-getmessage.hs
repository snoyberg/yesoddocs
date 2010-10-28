{-# LANGUAGE TypeFamilies, QuasiQuotes #-}
import Yesod
data Layout = Layout
mkYesod "Layout" [$parseRoutes|
/ RootR GET
/msg MsgR GET
|]
instance Yesod Layout where
    approot _ = ""
-- START
    defaultLayout contents = do
        PageContent title headTags bodyTags <- widgetToPageContent contents
        mmsg <- getMessage
        hamletToRepHtml [$hamlet|
!!!
%html
    %head
        %title $title$
        ^headTags^
    %body
        $maybe mmsg msg
            #message $msg$
        ^bodyTags^
|]
-- STOP
getRootR = defaultLayout [$hamlet|%a!href=@MsgR@ message|]
getMsgR = setMessage (string "foo") >> redirect RedirectTemporary RootR >> return ()
main = basicHandler 4000 Layout
