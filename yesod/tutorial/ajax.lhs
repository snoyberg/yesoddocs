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
> /static            StaticR Static siteStatic ajaxStatic
> |]

That third line there is the syntax for a subsite: Static is the datatype for the subsite argument; siteStatic returns the site itself (parse, render and dispatch functions); and ajaxStatic gets the subsite argument from the master argument.

Now, we'll define the Yesod instance. We'll still use a dummy approot value, but we're also going to define a default layout.

> instance Yesod Ajax where
>   approot _ = ""
>   defaultLayout content = do
>   Ajax pages _ <- getYesod
>   hamletToContent [$hamlet|
> !!!
> %html
>   %head
>     %title $pageTitle.content$
>     %link!rel=stylesheet!href=@stylesheet@
>     %script!src=$jquery$
>     %script!src=@script@
>     ^pageHead.content^
>   %body
>     %ul#navbar
>       $forall pages page
>         %li
>           %a!href=@PageR.pageSlug.page@ $cs.pageName.page$
>     #content
>       ^pageBody.content^
> |]
>     where
>       jquery = cs "http://ajax.googleapis.com/ajax/libs/jquery/1.4.2/jquery.min.js"
>       stylesheet = StaticR $ StaticRoute ["style.css"]
>       script = StaticR $ StaticRoute ["script.js"]

I know those last few functions look a little strange; this is to deal with how Hamlet works. Hamlet passes all functions the argument it receives, in this case the page content. When a function doesn't need that information, it must ignore its argument.

There's nothing Yesod-specific about the [style.css]($root/static/yesod/ajax/style.css) and [script.js]($root/static/yesod/ajax/script.js) files, so I won't describe them here.

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
>       page:_ -> applyLayoutJson (pageName page) (return ()) (html page) (json page)
>  where
>   html page = [$hamlet|
> %h1 $cs.pageName.page$
> %article $cs.pageContent.page$
> |]
>   json page = jsonMap
>       [ ("name", jsonScalar $ cs $ pageName page)
>       , ("content", jsonScalar $ cs $ pageContent page)
>       ]

We first try and find the appropriate Page, returning a 404 if it's not there. We then use the applyLayoutJson function, which is really the heart of this example. It allows you an easy way to create responses that will be either HTML or JSON, and which use the default layout in the HTML responses. It takes four arguments: 1) the title of the HTML page, 2) some value, 3) a function from that value to a Hamlet value, and 4) a function from that value to a Json value.

Under the scenes, the Json monad is really just using the Hamlet monad, so it gets all of the benefits thereof, namely interleaved IO and enumerator output. It is pretty straight-forward to generate JSON output by using the three functions jsonMap, jsonList and jsonMap. One thing to note: the input to jsonScalar must be HtmlContent; this helps avoid cross-site scripting attacks, by ensuring that any HTML entities will be escaped.

And now our typical main function. We need two parameters to build our Ajax value: the pages, and the static loader. We'll load up from a local directory.

> main :: IO ()
> main = do
>   pages <- loadPages
>   let static = fileLookupDir "static/yesod/ajax"
>   app <- toWaiApp $ Ajax pages static
>   basicHandler 3000 app
