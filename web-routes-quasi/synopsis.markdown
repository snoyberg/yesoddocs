---
title: Synopsis -- web-routes-quasi
---
The set of routes:

    /                Home       GET
    /entry/#String   EntryRoute GET
    /fake/#Integer   Fake
    /static          Static     Static siteStatic staticPath

will result in roughly the following Haskell code:

    data Routes = Home | EntryRoute String | Fake Integer | Static StaticRoutes

    parseRoutes :: [String] -> Either String Routes
    parseRoutes [] = Right Home
    parseRoutes ["entry", x] = EntryRoute x
    parseRoutes ["fake", x] | isInteger x = Fake $ read x
    parseRoutes ("static", rest) =
        case quasiParse siteStatic rest of
            Left s -> Left s
            Right x -> Right $ Static x
    parseRoutes _ = Left "Invalid URL"

    renderRoutes :: Routes -> [String]
    renderRoutes Home = []
    renderRoutes (EntryRoute x) = ["entry", x]
    renderRoutes (Fake x) = ["fake", show x]
    renderRoutes (Static s) = "static" : quasiRender siteStatic s

It will also generate a dispatch function which will call- when appropriate- functions with type signatures:

    getHome :: Handler
    getEntryRoute :: String -> Handler
    handlerFake :: Integer -> Handler

The Handler datatype is user-defined and controllable when calling the createRoutesQuasi function (see Haddock docs). These three functions are all wrapped up in a QuasiSite variable by that function as well.
