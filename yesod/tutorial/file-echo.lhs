> {-# LANGUAGE TypeFamilies, QuasiQuotes, TemplateHaskell #-}

> import Yesod
> import Data.Monoid (mempty)

> data Echo = Echo

> mkYesod "Echo" [$parseRoutes|
> / Homepage GET POST
> |]

> instance Yesod Echo where approot _ = ""

> getHomepage = defaultLayout $ do
>   setTitle $ string "Upload a file"
>   addBody [$hamlet|
> %form!method=post!action=.!enctype=multipart/form-data
>   File name:
>   %input!type=file!name=file
>   %input!type=submit
> |]

> postHomepage = do
>   rr <- getRequest
>   (_, files) <- liftIO $ reqRequestBody rr
>   fi <- maybe notFound return $ lookup "file" files
>   return [(fileContentType fi, toContent $ fileContent fi)]

> main = basicHandler 3000 Echo
