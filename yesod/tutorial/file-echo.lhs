> {-# LANGUAGE TypeFamilies, QuasiQuotes, TemplateHaskell #-}

> import Yesod

> data Echo = Echo

> mkYesod "Echo" [$parseRoutes|
> / Homepage GET POST
> |]

> instance Yesod Echo where approot _ = ""

> getHomepage = applyLayout "Upload a file" (return ()) [$hamlet|
> %form!method=post!action=.!enctype=multipart/form-data
>   File name:
>   %input!type=file!name=file
>   %input!type=submit
> |]

> postHomepage = do
>   rr <- getRequest
>   (_, files) <- liftIO $ reqRequestBody rr
>   fi <- maybe notFound return $ lookup "file" files
>   return [(contentTypeFromString $ cs $ fileContentType fi, toContent $ fileContent fi)]

> main = toWaiApp Echo >>= basicHandler 3000
