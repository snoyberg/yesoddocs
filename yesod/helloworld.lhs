---
title: Hello World -- Yesod
---
The following will serve the text "Hello World!" on port 3000 as plain text.

> {-# LANGUAGE TypeFamilies, QuasiQuotes, TemplateHaskell #-}
> import Yesod
> data HelloWorld = HelloWorld
> mkYesod "HelloWorld" [$parseRoutes|/ Home GET|]
> instance Yesod HelloWorld where approot _ = ""
> getHome = return $ RepPlain $ toContent "Hello World!"
> main = toWaiApp HelloWorld >>= basicHandler 3000
