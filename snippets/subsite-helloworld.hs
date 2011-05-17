-- START
{-# LANGUAGE QuasiQuotes, TypeFamilies, MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell, FlexibleInstances, OverloadedStrings #-}
import Yesod

-- Subsites have foundations just like master sites.
data HelloSub = HelloSub

-- We have a familiar analogue from mkYesod, with just one extra parameter.
-- We'll discuss that later.
mkYesodSub "HelloSub" [] [$parseRoutes|
/ SubRootR GET
|]

-- And we'll spell out the handler type signature.
getSubRootR :: Yesod master => GHandler HelloSub master RepHtml
getSubRootR = defaultLayout [$hamlet|Welcome to the subsite!|]

-- And let's create a master site that calls it.
data Master = Master
    { getHelloSub :: HelloSub
    }

mkYesod "Master" [$parseRoutes|
/ RootR GET
/subsite SubsiteR HelloSub getHelloSub
|]

instance Yesod Master where
    approot _ = ""

-- Spelling out type signature again.
getRootR :: GHandler sub Master RepHtml -- could also replace sub with Master
getRootR = defaultLayout [$hamlet|
<h1>Welcome to the homepage
<p>
    Feel free to visit the #
    <a href=@{SubsiteR SubRootR}>subsite
    \ as well.
|]

main = warpDebug 3000 $ Master HelloSub
