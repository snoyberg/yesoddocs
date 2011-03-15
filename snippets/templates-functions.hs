{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses, TemplateHaskell #-}
-- START
import Yesod

data HelloWorld = HelloWorld
type Author = String
type Title = String

mkYesod "HelloWorld" [$parseRoutes|
/ HomeR GET
/blog/#Author/#Title BlogR GET
|]

instance Yesod HelloWorld where
    approot _ = ""

getHomeR = defaultLayout $ do
    let myPhrase = "ring ring ring ring ring ring ring, BANANA PHONE"
    let myNumber = 12345
    addHamlet [$hamlet|
<h1>Welcome to the blog homepage.
<p
    We highly recommend that you read a blog post on #
    <a href=@{BlogR "einstein" "relativity"}>general relativity
    .
<p
    Also, the last 8 letters of the phrase #
    <i>#{myPhrase}
    \ are #{reverse $ take 8 $ reverse myPhrase}.
<p
    And the first two digits of #
    <i>#{show myNumber}
    \ are #{take 2 (show myNumber)}.
|]
-- STOP
getBlogR _ _ = return ()
main = warpDebug 3000 HelloWorld
