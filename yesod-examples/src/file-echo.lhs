> {-# LANGUAGE TypeFamilies, QuasiQuotes, TemplateHaskell, MultiParamTypeClasses #-}

> import Yesod
> import Data.Monoid (mempty)
> import qualified Data.ByteString.Char8 as S8

> data Echo = Echo

> mkYesod "Echo" [$parseRoutes|
> / Homepage GET POST
> |]

> instance Yesod Echo where approot _ = ""

> getHomepage = defaultLayout $ do
>   setTitle $ string "Upload a file"
>   addHamlet [$hamlet|
> %form!method=post!action=.!enctype=multipart/form-data
>   File name:
>   %input!type=file!name=file
>   %input!type=submit
> |]

> postHomepage = do
>   (_, files) <- runRequestBody
>   fi <- maybe notFound return $ lookup "file" files
>   return [(S8.pack $ fileContentType fi, toContent $ fileContent fi)]

> main = warpDebug 3000 Echo
