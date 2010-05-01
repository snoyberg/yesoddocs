---
title: AJAX -- Tutorials -- Yesod
---
We're going to write a very simple AJAX application. It will be a simple site with a few pages and a navbar; when you have Javascript, clicking on the links will load the pages via AJAX. Otherwise, it will use static HTML.

We're going to use jQuery for the Javascript, though anything would work just fine. Also, the AJAX responses will be served as JSON. Let's get started.

> {-# LANGUAGE TypeFamilies, QuasiQuotes, TemplateHaskell #-}
> import Yesod
> import Yesod.Helpers.Static

Like the [blog tutorial](blog.html), we'll define some data first.

> data Page = Page
>   { pageName :: String
>   , pageSlug :: String
>   , pageContent :: String
>   }

> loadPages :: IO [Page]
> loadPages = return
>   [ Page "Page 1" "page-1" "My first page"
>   , Page "Page 2" "page-2" "My second page"
>   , Page "Page 3" "page-3" "My third page"
>   ]

> data Ajax = Ajax
>   { ajaxPages :: [Page]
>   , ajaxStatic :: Static
>   }

Now the routes; we'll have a homepage, a pattern for the pages, and use a static subsite for the Javascript and CSS files.

> mkYesod "Ajax" [$parseRoutes|
> /                  HomeR   GET
> /page/$page        PageR   GET
> /static            StaticR StaticRoutes siteStatic ajaxStatic
> |]

That third line there is the syntax for a subsite: StaticRoutes is the datatype for the subsite URLs; siteStatic returns the site itself (parse, render and dispatch functions); and ajaxStatic gets the subsite argument from the master argument.

Now, we'll define the Yesod instance. We'll still use a dummy approot value, but we're also going to define a default layout.

> instance Yesod Ajax where
>   approot _ = ""
>   defaultLayout (Ajax pages' _) content _ = template content where
>       template = [$hamlet|
> !!!
> %html
>   %head
>     %title $pageTitle$
>     %link!rel=stylesheet!href=@stylesheet@
>     %script!src=@script@ FIXME
>     ^pageHead^
>   %body
>     %ul#navbar
>       $forall pages page
>         %li
>           %a!href=@page.pageUrl@ $page.pageName.cs$
>     #content
>       ^pageBody^
> |]
>       pages _ = pages'
>       stylesheet _ = StaticR $ toStaticRoute ["style.css"]
>       script _ = StaticR $ toStaticRoute ["script.js"]
>       pageUrl = PageR . pageSlug

I know those last few functions look a little strange; this is to deal with how Hamlet works. Hamlet passes all functions the argument it receives, in this case the page content. When a function doesn't need that information, it must ignore its argument.

FIXME: style.css and script.js

Now we need our handler functions. We'll have the homepage simply redirect to the first page, so:

> getHomeR :: Handler Ajax ()
> getHomeR = do
>   Ajax pages _ <- getYesod
>   let first = head pages
>   redirect RedirectTemporary $ PageR $ pageSlug first

And now the cool part: a handler that returns either HTML or JSON data, depending on the request headers.

> getPageR :: String -> Handler Ajax RepHtmlJson
> getPageR slug = do
>   Ajax pages _ <- getYesod
>   case filter (\e -> pageSlug e == slug) pages of
>       [] -> notFound
>       page:_ -> applyLayoutJson (pageName page) page html json
>  where
>   html = [$hamlet|
> %h1 $pageName.cs$
> %article $pageContent.cs$
> |]
>   json page = jsonMap
>       [ (jsonScalar $ cs "name", jsonScalar $ cs $ pageName page)
>       , (jsonScalar $ cs "content", jsonScalar $ cs $ pageContent page)
>       ]

FIXME: more explanation.

And now our typical main function. We need two parameters to build our Ajax value: the pages, and the static loader. We'll load up from a local directory.

> main :: IO ()
> main = do
>   pages <- loadPages
>   let static = fileLookupDir "static/yesod/ajax"
>   app <- toWaiApp $ Ajax pages static
>   basicHandler 3000 app
