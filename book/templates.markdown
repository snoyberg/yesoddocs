---
title: Templates
---
The templating system is Yesod is called Hamlet. This book contains a [full reference](hamlet.html) chapter on it, so this chapter will be more about showing the usage of Hamlet. It will also show Hamlet's sister languages, Cassius and Julius, which produce CSS and Javascript, respectively.

    1 {-# LANGUAGE TypeFamilies, QuasiQuotes #-}
    2 import Yesod
    3 data HelloWorld = HelloWorld
    4 mkYesod "HelloWorld" [$parseRoutes|/ HomeR GET|]
    5 instance Yesod HelloWorld where approot _ = ""
    6 getHomeR = hamletToRepHtml [$hamlet|Hello World!|]
    7 main = basicHandler 3000 HelloWorld

Excepting line 6, this is identical to the Hello World from the previous chapter. The [$hamlet| and |] stuff introduces some *quasi-quotation*, which allows us to embed an arbitrary string and have it converted to Haskell code. We'll see some other ways of using Hamlet as well.

hamletToRepHtml does exactly what it sounds like; it converts a value of type Hamlet to a RepHtml. So this example is really the first time we're serving an HTML page from our application.

<div class="advanced">

So what's a value of type Hamlet? It's actually a little bit complicated:

    type Hamlet url = (url -> [(String, String)] -> String) -> Html

The datatype Html is simply a way of building up text values, based upon the incredibly fast blaze-builder library. That first argument is the tricky part: it's a *URL rendering function*. Given a type-safe URL and a list of query-string parameter pairs, it produces URL. For the most part, you can forget about the details, as Yesod automatically provides the correct parameters and Hamlet calls that rendering function for you.

</div>

But that's a really boring HTML page: it's just the text "Hello World!" without any tags! Let's see something a little bit more interesting:

    getHomeR = hamletToRepHtml [$hamlet|
    %h1 Hello World!
    %p Here are some of my favorite links:
    %ul
        %li
            %a!href="http://docs.yesodweb.com/" Yesod Web Framework Docs
        %li
            %a!href="http://www.haskell.org/" Haskell Homepage
    %p Thanks for visiting!
    |]

Let's see how we could spice up the names example from the previous chapter:

    getNameR name = hamletToRepHtml [$hamlet|
    %h1 Welcome to the Names Webapp
    %p You want me to tell you your name? Fine, it's $name$.
    |]

Notice that we include the name in the template by surrounding it with the dollar sign. A nifty feature is that if you include Strings in this manner, they are automatically HTML-entity escaped. That's right: you automatically avoid a large vector of XSS attacks.

We also have at-sign (@) interpolation, which allows us to embed type-safe URLs:

    [$hamlet|
    %a!href=@HomeR@ Return to homepage
    |]

And you can also do basic function application, such as:

    let michael = "Michael"
    hamletToRepHtml [$hamlet|
    %a!href=@NameR.michael@ See Michael's page
    |]

## Using defaultLayout

The examples above are missing most of the common pieces of an HTML page: a html head, title and body tags, as well as a doctype. It would not only be tedious to have to include all of that stuff for every page, but would also introduce a lot of repetition of code and make it difficult to have a uniform layout for our pages. Let's see how to use defaultLayout as a drop-in replacement for hamletToRepHtml:

    getHomeR = defaultLayout $ do
        setTitle "Home Page"
        addBody [$hamlet|%h1 Hello World|]

<p class="advanced">This example is actually using widgets, a concept we will cover in full in the [widgets chapter](widgets.html). For the moment, we'll just be treating the widget functions as magic without really explaining them.</p>

Assuming defaultLayout has not been overridden with a custom implementation, the resulting HTML would look like:

    <!DOCTYPE html>
    <html>
        <head>
            <title>Home Page</title>
        </head>
        <body>
            <h1>Hello World</h1>
        </body>
    </html>

## Cassius

We can also add CSS declarations right along with defaultLayout:

    getHomeR = defaultLayout $ do
        setTitle "Home Page"
        addHtml [$hamlet|%h1 Hello World|]
        addStyle [$cassius|
        h1
            color: green
        |]

Cassius, like Hamlet, uses a whitespace sensitive formatting. It also allows variable interpolation like Hamlet, allowing you to avoid typos in class names like this:

    getHomeR = defaultLayout $ do
        let myClass = "some-missspelled-class-name"
        addHtml [$hamlet|%h1.myClass Hello World|]
        addStyle [$cassius|
        h1.$myClass$
            color: green
        |]

## Julius

And just so that Javascript doesn't feel left out, we have Julius. Julius is basically a straight pass-through: it provides no syntax help. It just allows you to do variable interpolation, so our names example might become:

    getNamesR name = defaultLayout $ do
        addJulius [$julius|alert("Hello %name%");|]

Notice that we use percent signs instead of dollar signs for Julius; since jQuery uses dollar signs extensively, it would be very tedious to need to escape dollar signs constantly.

## Overriding defaultLayout

defaultLayout is a method of the Yesod typeclass, so you can simply provide an alternative implementation there. Yesod internally uses the defaultLayout method whenever possible: error messages, the built-in subsites, etc. This means that a lot of the built in features will automatically get styled like the rest of your site. A simply implementation is:

    defaultLayout widget = do
        PageContent title headTag bodyTag <- widgetToPageContent widget
        hamletToRepHtml [$hamlet|
        !!!
        %html
            %head
                %title $title$
                ^headTag^
            %body
                %h1 Our Website Banner
                %h2 And Our Silly Slogan
                ^bodyTag^
                %p And some footer with copyright information
        |]

The headTag variable will automatically have all of the CSS and Javascript declarations stated above. You can even introduce your own CSS and Javascript in the defaultLayout function:

    defaultLayout widget = do
        PageContent title headTag bodyTag <- widgetToPageContent $ do
            widget
            addStyle [$cassius|
            h1
                color: red
            |]
        hamletToRepHtml [$hamlet|...|]
