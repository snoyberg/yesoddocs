The templating system in Yesod is called Hamlet. This book contains a [full reference](hamlet.html) chapter on it, so this chapter will be more about showing the usage of Hamlet. It will also show Hamlet's sister languages, Cassius and Julius, which produce CSS and Javascript, respectively.

~templates-helloworld

Excepting line 6, this is identical to the Hello World from the previous chapter. The [$hamlet| and |] stuff introduces some *quasi-quotation*, which allows us to embed an arbitrary string and have it converted to Haskell code. We'll see some other ways of using Hamlet as well.

hamletToRepHtml does exactly what it sounds like; it converts a value of type Hamlet to a RepHtml. So this example is really the first time we're serving an HTML page from our application.

<div class="advanced">

So what's a value of type Hamlet? It's actually a little bit complicated:

    type Hamlet url = (url -> [(String, String)] -> String) -> Html

The datatype Html is simply a way of building up text values, based upon the incredibly fast blaze-builder library. That first argument is the tricky part: it's a *URL rendering function*. Given a type-safe URL and a list of query-string parameter pairs, it produces URL. For the most part, you can forget about the details, as Yesod automatically provides the correct parameters and Hamlet calls that rendering function for you.

</div>

But that's a really boring HTML page: it's just the text "Hello World!" without any tags! Let's see something a little bit more interesting:

~templates-helloworld2

Let's see how we could spice up the names example from the previous chapter:

~templates-names

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

defaultLayout is a method of the Yesod typeclass, so you can simply provide an alternative implementation there. Yesod internally uses the defaultLayout method whenever possible: error messages, the built-in subsites, etc. This means that a lot of the built in features will automatically get styled like the rest of your site. A simple implementation is:

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

<p class="advanced">There's actually much more to the embedding of CSS and Javascript than I have implied. By default, the raw code is inserted into your HTML via style and script tags. However, Yesod also has the ability to automatically create static files and serve the code externally. The site template created with the scaffolding tool does all of this for you.</p>

### getMessage

A common requirement in web applications is setting a message during one request and displaying it during another. For example, let's say you are logging into a site and type the wrong password. The application would like to show a message saying "invalid password" and allow you to type in your username/password again. There are basically two approaches to this:

1) In the handler that checks for a valid password, return a login form with the error message whenever an invalid password is submitted.

2) The handler that checks for a valid password redirects to the original login form when there is an invalid password.

At first glance option 1 sounds preferable, since it avoids an extra HTTP request. However, in general it's a good idea to avoid returning content from a POST request, since it can complicate back/forward button usage and make refreshing a page tricky. The problem with the second approach is that we need some way to tell the original login form page to display the error message.

Yesod provides a built in set of functions: getMessage and setMessage. We will cover these in more detail when discussing sessions, but for now it's enough to know that your defaultLayout should display the message returned by getMessage. Not only will this simplify your application code by just having to call getMessage once, but it will make interoperation with built-in Yesod features much better. A basic example would be:

    defaultLayout widget = do
        PageLayout title headTag bodyTag <- widgetToPageLayout widget
        mmsg <- getMessage
        hamletToRepHtml [$hamlet|
        !!!
        %html
            %head
                %title $title$
                ^headTag^
            %body
                $maybe mmsg msg
                    #message $msg$
                ^bodyTag^
        |]

## External template files

Quasi-quoting your templates can be convenient, as no extra files are needed, your template is close to your code, and recompilation happens automatically whenever your template changes. On the other hand, this also clutters your Haskell code with templates and requires a recompile for any change in the template. Hamlet provides two sets of functions for including an external template:

1) hamletFile/cassiusFile/juliusFile

2) hamletFileDebug/cassiusFileDebug/juliusFileDebug

What's very nice about these is that they have the exact same type signature, so they can be exchanged without changing your code otherwise. These functions are not exported by the Yesod module and must be imported directly from their respective modules (Text.Hamlet, Text.Cassius, Text.Julius). You'll see in a second why that is.

Usage is very straight-forward. Assuming there is a Hamlet template stored in "my-template.hamlet", you could write:

    hamletToRepHtml $(hamletFile "my-template.hamlet")

For those not familiar, the dollar sign and parantheses indicate a Template Haskell interpolation. Using the second set of functions, the above would become:

    hamletToRepHtml $(hamletFileDebug "my-template.hamlet")

So why do we have two sets of functions? The first fully embeds the contents of the template in the code at compile time and never looks at the template again until a recompile. This is ideal for a production environment: compile your code and you have no runtime dependency on any template files. It also avoids a runtime penalty of needing to read a file.

The debug set of functions is intended for development. These functions work a little bit of magic: at compile time, they inspect your template, determine which variables they reference, and generate some Haskell code to load up those variables. At run time, they read in the template again and feed in those variables. This has a number of implications:

* Changes to your template become immediately visible upon saving the file, no recompile required.

* If you introduce new variables to the template that were not there before, you'll need to recompile. This might require you manually nudging GHC to recompile the Haskell file, since it won't think anything has changed.

* Due to some of the tricks needed to pull this off, some of the more corner cases of templates are not supported. For example, using a forall to bind a function to a variable. This is an obscure enough case that it shouldn't be an issue.

This is also the reason why Yesod does not export these functions by default. The Yesod scaffolding tool creates a Settings.hs file which exports these functions, in a slightly modified form, and chooses whether to use the debug or regular version based upon build flags. Long story short: it automatically uses the debug version during development and non-debug version during production.

Excepting very short templates, this is probably how you'll write most of your templates in Yesod. The typical file structure is to create hamlet, cassius and julius folders and place the respective templates in each. Each template has a filename extension matching the template language. In other words, you'd typically have:

    # hamlet/homepage.hamlet
    %h1 Hello World!

    # cassius/homepage.cassius
    h1
        color: green

    # julius/homepage.julius
    alert("Don't you hate it when you get an alert when you open a page?");
    
    # Settings.hs, paraphrasing
    import qualified Text.Hamlet
    import qualified Text.Cassius
    import qualified Text.Julius
    
    hamletFile x = Text.Hamlet.hamletFileDebug $ "hamlet/" ++ x ++ ".hamlet"
    -- same for cassius and julius
    -- when moving to production, you would just remove Debug

    # And finally your handler code
    import Settings
    getHomeR = defaultLayout $ do
        setTitle "Homepage"
        addBody $(hamletFile "homepage")
        addStyle $(cassiusFile "homepage")
        addJavascript $(juliusFile "homepage")

## Summary

Yesod has templating languages for HTML, CSS and Javascript. All of them allow variable interpolation, safe handling of URLs and embedding sub-templates. Since the code is dealt with at compile time, you can use the compiler as your friend and get strong type safety guarantees. Oh, and XSS vulnerabilities get handled automatically.

There are three ways to embed the templates: through quasi-quotation, regular external and debug external. Quasi-quotation is great for small, simple templates that won't be changing often. Debug mode is great for development, and since it has the same type signature as the regular external functions, you can easily switch to using them for your production code.

By using built-in Yesod constructs like defaultLayout and getMessage, you'll get a consistent look-and-feel throughout your site, including pages automatically generated by Yesod such as error pages and authentication.
