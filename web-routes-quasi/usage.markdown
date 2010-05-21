---
title: Usage -- web-routes-quasi
---
Most of the time, there are only two functions you'll need from this package: parseRoutes and createQuasiRoutes. The former is a quasi-quoter that converts [the web-routes-quasi syntax](syntax.html) into a list of resources; the latter declares datatypes and functions based on a list of resources.

The createQuasiRoutes function is necessarily quite complicated; please see the haddock documentation. In an ideal world, normal users will never need to use that function directly, as web frameworks will provide wrappers around it. For example, Yesod does this.

However, users will always need to write their own handlers. When calling createQuasiRoutes, you will need to provide some datatype for handler functions to return; for our purposes here, we will assume that this datatype is Handler. You also need an argument datatype; we'll assume that it is MyArgs. Finally, let's assume that we have the following routes ([see the syntax page for explanation](syntax.html)):

    /                        Home    GET
    /user/#UserId            User    GET POST
    /auth                    Auth    AuthRoutes authSite getAuthArgs
    /page/#PageName          Page
    /static/*Strings         Static  GET

You would need to provide functions with the following type signatures:

    getHome :: Handler
    getUser :: UserId -> Handler
    postUser :: UserId -> Handler
    authSite :: QuasiSite Application AuthArgs masterArgs
    getAuthArgs :: MyArgs -> AuthArgs
    handlePage :: PageName -> Handler
    getStatic :: [String] -> Handler
