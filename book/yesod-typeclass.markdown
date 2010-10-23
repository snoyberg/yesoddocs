# Outline

* Purpose of site argument
* Used for settings
* Only approot needs to be overriden
* defaultLayout used by built in stuff (error messages, subsites, etc)
* Custom errorHandler example
* Sessions (encryptKey- horizontal scaling, clientSessionDuration)
* Logging with onRequest
* Some special features for static files (addStaticContent, urlRenderOverride)
* Auth stuff (isAuthorized, isWriteRequest, authRoute)
* Customize URLs (splitPath, joinPath, examples)

In the last chapter we showed some example Yesod applications and explained that the foundation datatype needed to be an instance of the Yesod typeclass. This typeclass is a central place to declare settings for your Yesod application. Besides approot, all of these settings are optional, but they 
