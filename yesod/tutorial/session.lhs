**NOTE: This tutorial requires the development version of Yesod (version 0.4.0). The [tutorial main page]($root/yesod/tutorial/) has instructions on setting up your environment.**

> {-# LANGUAGE TypeFamilies, QuasiQuotes, TemplateHaskell #-}
> import Yesod
> import Control.Applicative ((<$>), (<*>))
> 
> data Session = Session
> mkYesod "Session" [$parseRoutes|
> / Root GET POST
> |]
> getRoot :: Handler Session RepHtml
> getRoot = do
>     sess <- reqSession `fmap` getRequest
>     hamletToRepHtml [$hamlet|
> %form!method=post
>     %input!type=text!name=key
>     %input!type=text!name=val
>     %input!type=submit
> %h1 $show.sess$
> |]
> 
> postRoot :: Handler Session ()
> postRoot = do
>     (key, val) <- runFormPost' $ (,) <$> stringInput "key" <*> stringInput "val"
>     setSession key val
>     liftIO $ print (key, val)
>     redirect RedirectTemporary Root
> 
> instance Yesod Session where
>     approot _ = ""
>     clientSessionDuration _ = 1
> main = basicHandler 3000 Session
