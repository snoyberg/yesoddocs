> {-# LANGUAGE TypeFamilies, QuasiQuotes, OverloadedStrings #-}
> import Yesod
> import Yesod.Widget
> import Yesod.Helpers.Static
> import Yesod.Form.Jquery
> import Yesod.Form.Nic
> import Control.Applicative
> 
> data HW = HW { hwStatic :: Static }
> type Handler = GHandler HW HW
> mkYesod "HW" [$parseRoutes|
> / RootR GET
> /form FormR
> /static StaticR Static hwStatic
> /autocomplete AutoCompleteR GET
> |]
> instance Yesod HW where approot _ = ""
> instance YesodJquery HW
> instance YesodNic HW
> wrapper h = [$hamlet|
> #wrapper ^h^
> %footer Brought to you by Yesod Widgets&trade;
> |]
> getRootR = defaultLayout $ flip wrapWidget wrapper $ do
>     i <- newIdent
>     setTitle $ string "Hello Widgets"
>     addStyle [$cassius|
>   #$i$
>       color: red|]
>     addStylesheet $ StaticR $ StaticRoute ["style.css"] []
>     addStylesheetRemote "http://localhost:3000/static/style2.css"
>     addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/1.4.2/jquery.min.js"
>     addScript $ StaticR $ StaticRoute ["script.js"] []
>     addBody [$hamlet|
> %h1#$i$ Welcome to my first widget!!!
> %p
>     %a!href=@RootR@ Recursive link.
> %p
>     %a!href=@FormR@ Check out the form.
> %p.noscript Your script did not load. :(
> |]
>     addHead [$hamlet|%meta!keywords=haskell|]
> 
> handleFormR = do
>     (res, form, enctype) <- runFormPost $ fieldsToTable $ (,,,,,,,,)
>         <$> stringField "My Field" Nothing
>         <*> stringField "Another field" (Just "some default text")
>         <*> intField "A number field" (Just 5)
>         <*> jqueryDayField "A day field" Nothing
>         <*> timeField "A time field" Nothing
>         <*> boolField "A checkbox" (Just False)
>         <*> jqueryAutocompleteField AutoCompleteR "Autocomplete" Nothing
>         <*> nicHtmlField "HTML"
>                 (Just $ string "You can put <rich text> here")
>         <*> maybeEmailField "An e-mail addres" Nothing
>     let mhtml = case res of
>                     FormSuccess (_, _, _, _, _, _, _, x, _) -> Just x
>                     _ -> Nothing
>     defaultLayout $ do
>         addStyle [$cassius|
> .tooltip
>     color: #666
>     font-style: italic
> textarea.html
>     width: 300px
>     height: 150px|]
>         wrapWidget form $ \h -> [$hamlet|
> %form!method=post!enctype=$enctype$
>     %table
>         ^h^
>         %tr
>             %td!colspan=2
>                 %input!type=submit
>     $maybe mhtml html
>         $html$
> |]
>         setTitle $ string "Form"
> 
> main = basicHandler 3000 $ HW $ fileLookupDir "static" typeByExt
> 
> getAutoCompleteR :: Handler RepJson
> getAutoCompleteR = do
>     term <- runFormGet' $ stringInput "term"
>     jsonToRepJson $ jsonList
>         [ jsonScalar $ term ++ "foo"
>         , jsonScalar $ term ++ "bar"
>         , jsonScalar $ term ++ "baz"
>         ]
