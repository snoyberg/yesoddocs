---
title: Multi-lingual -- Tutorials -- Yesod
---

> {-# LANGUAGE QuasiQuotes #-}
> {-# LANGUAGE TemplateHaskell #-}
> {-# LANGUAGE TypeFamilies #-}

> import Yesod
> import Data.Monoid (mempty)

> data I18N = I18N

> mkYesod "I18N" [$parseRoutes|
> /            HomepageR GET
> /set/#String SetLangR  GET
> |]

> instance Yesod I18N where
>     approot _ = "http://localhost:3000"

> getHomepageR :: Handler I18N RepHtml
> getHomepageR = do
>     ls <- languages
>     let hello = chooseHello ls
>     let choices =
>             [ ("en", "English")
>             , ("es", "Spanish")
>             , ("he", "Hebrew")
>             ]
>     applyLayout "I18N Homepage" mempty [$hamlet|
> %h1 $hello$
> %p In other languages:
> %ul
>     $forall choices choice
>         %li
>             %a!href=@SetLangR.fst.choice@ $snd.choice$
> |]

> chooseHello :: [String] -> String
> chooseHello [] = "Hello"
> chooseHello ("he":_) = "שלום"
> chooseHello ("es":_) = "Hola"
> chooseHello (_:rest) = chooseHello rest

> getSetLangR :: String -> Handler I18N ()
> getSetLangR lang = do
>     setLanguage lang
>     redirect RedirectTemporary HomepageR

> main :: IO ()
> main = toWaiApp I18N >>= basicHandler 3000
