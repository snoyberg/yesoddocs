# Introduction

Yesod is built upon the [hamlet package](http://hackage.haskell.org/package/hamlet), which provides three templating systems: Hamlet (HTML), Cassius (CSS) and Julius (Javascript). These systems are all available for use outside the realm of Yesod. In this chapter, we will be demonstrating their usage in a standalone context.

Hamlet in particular has a number of features that won't be covered in this chapter. Instead, there is a [Hamlet reference chapter](/book/hamlet/) in the appendix which covers all functionality. This chapter should get you familiar with the syntax, usage and basic features.

# Type Safety

One of the biggest features of Yesod is its pervasive type safety. This was the original impetus for the creation of Hamlet. As such, all variable interpolation gets checked at compile time. Hamlet supports three forms of interpolation:

* Any instance of the ToHtml typeclass. This includes String and Html. This is also where Hamlet shows great <abbr title="Cross Site Scripting">XSS</abbr> attack defense: whenever interpolating a String, Hamlet automatically escapes all HTML entities. This is called dollar-sign interpolation.

* Type-safe URLs. We mentioned in [the basics chapter](/book/basics/) that each URL can be represented as a Haskell value, and then we use a render function to convert those values into strings. Hamlet allows you to interpolate those type-safe URLs directly. This is called at-sign interpolation.

* Other Hamlet templates. This is great for creating a chunk of code that will be reused in other templates. This is called caret interpolation.

Cassius provides support for the first two forms of interpolation, but instead of the ToHtml typeclass, there is the ToCss typeclass. Caret interpolation does not make as much sense with CSS. Julius provides support for all three; however, instead of dollar-sign interpolation, Julius uses the percent sign. This is to make it more natural to deal with the jQuery Javascript library, which has a special meaning for the dollar sign.

<p class="advanced">Cassius used to provide support for caret interpolation via something called mixins. However, this required a lot of complexity in the implementation and usage, and was not used very often. It may reappear at some point in the future.</p>

In all three languages, when you want to print an interpolation character, you enter it twice. For example, in Hamlet:

    The price of this item is $$4.50.

# Quasi-quotation

One way to 

<hr>

<div class="advanced">

Just a little heads-up on some of the examples below: in order to simplify the examples below, we're going to assume the OverloadedStrings language extension. If you rather not use this extension, you'll need to make some minor modifications, such as replacing:

    setTitle "Home Page"

with

    setTitle $ string "Home Page"

</div>

The templating system in Yesod is called Hamlet. This book contains a [full reference](/book/hamlet/) chapter on it, so this chapter will be more about showing the usage of Hamlet. It will also show Hamlet's sister languages, Cassius and Julius, which produce CSS and Javascript, respectively.

~templates-helloworld

__Note: the rest of this chapter reflects a previous version of the book. The content still must be updated; I appologize for the confusion. Nonetheless, the material is accurate.__

Excepting line 6, this is identical to the Hello World from the chapter. The [$hamlet| and |] stuff introduces some *quasi-quotation*, which allows us to embed an arbitrary string and have it converted to Haskell code. We'll see some other ways of using Hamlet as well.

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
        addHamlet [$hamlet|%h1 Hello World|]

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
        addHamlet [$hamlet|%h1 Hello World|]
        addCassius [$cassius|
        h1
            color: green
        |]

Cassius, like Hamlet, uses a whitespace sensitive formatting. It also allows variable interpolation like Hamlet, allowing you to avoid typos in class names like this:

    getHomeR = defaultLayout $ do
        let myClass = "some-missspelled-class-name"
        addHamlet [$hamlet|%h1.myClass Hello World|]
        addCassius [$cassius|
        h1.$myClass$
            color: green
        |]

## Julius

And just so that Javascript doesn't feel left out, we have Julius. Julius is basically a straight pass-through: it provides no syntax help. It just allows you to do variable interpolation, so our names example might become:

    getNamesR name = defaultLayout $ do
        addJulius [$julius|alert("Hello %name%");|]

Notice that we use percent signs instead of dollar signs for Julius; since jQuery uses dollar signs extensively, it would be very tedious to need to escape dollar signs constantly.

## External template files

Quasi-quoting your templates can be convenient, as no extra files are needed, your template is close to your code, and recompilation happens automatically whenever your template changes. On the other hand, this also clutters your Haskell code with templates and requires a recompile for any change in the template. Hamlet provides two sets of functions for including an external template:

1) hamletFile/cassiusFile/juliusFile

2) hamletFileDebug/cassiusFileDebug/juliusFileDebug

What's very nice about these is that they have the exact same type signature, so they can be exchanged without changing your code otherwise. These functions are not exported by the Yesod module and must be imported directly from their respective modules (Text.Hamlet, Text.Cassius, Text.Julius). You'll see in a second why that is.

Usage is very straight-forward. Assuming there is a Hamlet template stored in "my-template.hamlet", you could write:

    hamletToRepHtml $(hamletFile "my-template.hamlet")

For those not familiar, the dollar sign and parantheses indicate a Template Haskell interpolation. Using the second set of functions, the above would become:

    hamletToRepHtml $(hamletFileDebug "my-template.hamlet")

Note that in order to use this, you need to enable the TemplateHaskell language extension. You can do so by adding the following line to the top of your source file:

    {-# LANGUAGE TemplateHaskell #-}

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
        addHamlet $(hamletFile "homepage")
        addCassius $(cassiusFile "homepage")
        addJulius $(juliusFile "homepage")

For simplicity, most of the examples in this book will use quasi-quoted syntax. Just remember that you can always swap this out for external files.

# Summary

Yesod has templating languages for HTML, CSS and Javascript. All of them allow variable interpolation, safe handling of URLs and embedding sub-templates. Since the code is dealt with at compile time, you can use the compiler as your friend and get strong type safety guarantees. Oh, and XSS vulnerabilities get handled automatically.

There are three ways to embed the templates: through quasi-quotation, regular external and debug external. Quasi-quotation is great for small, simple templates that won't be changing often. Debug mode is great for development, and since it has the same type signature as the regular external functions, you can easily switch to using them for your production code.
