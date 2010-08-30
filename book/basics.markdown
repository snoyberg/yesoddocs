---
title: Basics
---
Yesod comes with a scaffolding tool that creates a basic site template for you. This takes care of a lot of the tedious, boilerplate code you need to get started: writing a cabal file, creating default layout templates, a directory structure, etc. It's the best way to get started on a site. However, for learning Yesod, we'll start off without it. This will give you a much better feel for *how* Yesod works.

So it proper tradition, we'll start with Hello World.

    1 {-# LANGUAGE TypeFamilies, QuasiQuotes #-}
    2 import Yesod
    3 data HelloWorld = HelloWorld
    4 mkYesod "HelloWorld" [$parseRoutes|/ Home GET|]
    5 instance Yesod HelloWorld where approot _ = ""
    6 getHome = return $ RepPlain $ toContent "Hello World!"
    7 main = basicHandler 3000 HelloWorld

Line 1 simply turns on some language extensions, and line 2 imports the Yesod library. Line 3 creates a new datatype called HelloWorld. Every Yesod application has a single data type at its core. This is a place where you can store settings, keep initialization values, or anything else. We don't use any of those features in our simple example.

This central type- called the **site argument**- plays another role as well: it allows for a lot of type safety. 
