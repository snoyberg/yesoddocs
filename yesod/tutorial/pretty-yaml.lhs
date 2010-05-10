---
title: Pretty YAML -- Tutorials -- Yesod
---
Sorry, I didn't have a chance to comment this one yet. However, the goal is simply to convert a YAML file into pretty HTML. Enjoy!

> {-# LANGUAGE TypeFamilies, QuasiQuotes, TemplateHaskell #-}

> import Yesod
> import Data.Object.Yaml
> import Web.Encodings
> import qualified Data.ByteString as B
> import qualified Data.ByteString.Lazy as L
> import Data.Object.String

> data PY = PY

> mkYesod "PY" [$parseRoutes|
> / Homepage GET POST
> |]

> instance Yesod PY where
>     approot _ = "http://localhost:3000"

> template :: Monad m => Maybe (Hamlet url m ()) -> Hamlet url m ()
> template myaml = [$hamlet|
> !!!
> %html
>     %head
>         %meta!charset=utf-8
>         %title Pretty YAML
>     %body
>         %form!method=post!action=.!enctype=multipart/form-data
>             File name:
>             %input!type=file!name=yaml
>             %input!type=submit
>         $maybe myaml yaml
>             %div ^yaml^
> |]

> getHomepage :: Handler PY RepHtml
> getHomepage = hamletToRepHtml $ template Nothing

> --FIXMEpostHomepage :: Handler PY RepHtmlJson
> postHomepage :: Handler PY RepHtml
> postHomepage = do
>     rr <- getRequest
>     (_, files) <- liftIO $ reqRequestBody rr
>     fi <- case lookup "yaml" files of
>             Nothing -> invalidArgs [("yaml", "Missing input")]
>             Just x -> return x
>     so <- liftIO $ decode $ B.concat $ L.toChunks $ fileContent fi
>     {- FIXME
>     let ho' = fmap Text to
>     templateHtmlJson "pretty-yaml" ho' $ \ho ->
>         return . setHtmlAttrib "yaml" (Scalar $ cs ho :: HtmlObject)
>     -}
>     --let ho = cs (so :: StringObject) :: HtmlObject
>     hamletToRepHtml $ template $ Just $ objToHamlet so

> objToHamlet :: Monad m => StringObject -> Hamlet url m ()
> objToHamlet (Scalar s) = [$hamlet|$cs.s$|]
> objToHamlet (Sequence list) = [$hamlet|
> %ul
>     $forall list o
>         %li ^objToHamlet.o^
> |]
> objToHamlet (Mapping pairs) = [$hamlet|
> %dl
>     $forall pairs pair
>         %dt $cs.fst.pair$
>         %dd ^objToHamlet.snd.pair^
> |]

> main :: IO ()
> main = toWaiApp PY >>= basicHandler 3000
