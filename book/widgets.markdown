# Introduction

One of the challenges in web development is that we have to coordinate three different client-side technologies: HTML, CSS and Javascript. Worse still, we have to place these components in different locations on the page: CSS in a style tag in the head, Javascript in a script tag in the head, and HTML in the body. And never mind if you want to put your CSS and Javascript in separate files!

In practice, this works out fairly nicely when building a single page, becuase we can separate our structure (HTML), styling (CSS) and logic (Javascript). But when we want to build modular pieces of code that can be easily composed, it can be a headache to coordinate all three pieces separately. Widgets are Yesod's solution to the problem. They also help with the issue of including libraries, such as jQuery, one time only.

# Expanding Hello World

In the [basics chapter](/book/basics/), our Hello World example produced some very basic HTML:

    <!DOCTYPE html> 
    <html><head><title></title></head><body>Hello World!</body></html>

Noticeably missing is a title. Let's fix that:

~widgets-settitle

This produces exactly what we'd expect:

    <!DOCTYPE html> 
    <html><head><title>Hello World</title></head><body>Hello World!</body></html>

defaultLayout takes a Widget as its argument; a Widget is a monad that contains information on the title, head tags, styles, javascript and body that make up a document. defaultLayout breaks all of this up into pieces and places them into an HTML template file.

<p class="advanced">OK, two things: there is no such thing as a Widget in Yesod; it's a GWidget. We'll see why when we learn about the default site template. And defaultLayout is a method in the Yesod typeclass, which you can (and should) override to give your site a uniform theme. We'll discuss that in the [Yesod typeclass chapter](/book/yesod-typeclass/).</p>

When we place a hamlet template in a widget, the content gets appended to the body section of the page. We have other helper functions, such as setTitle, that place content elsewhere. Let's say we decide we would like our text to be red. One approach would be to use a style attribute in our Hamlet:

~widgets-style-attribute

But the better approach would be to declare this information in a separate CSS section, either a style tag or an external stylesheet.

## Polymorphic Hamlet

The hamlet quasi-quoter is special in that its result can be polymorphic. So line 3 of the previous example could read:

    addWidget [$hamlet|%p!style="color:red" Hello World!|]

or

    addHamlet [$hamlet|%p!style="color:red" Hello World!|]

or

    addHtml [$hamlet|%p!style="color:red" Hello World!|]

Without changing the resulting HTML page. It might be useful to look at the type signatures of these three functions:

    addWidget :: Widget () -> Widget ()
    addHamlet :: Hamlet    -> Widget ()
    addHtml   :: Html      -> Widget ()

addWidget is actually just <code>id</code> in disguise; it can be useful for forcing a specific type, and you'll see below that it can make your widget code look more consistent. You may be wondering why we need all three of these. Sometimes, it is easier to get a hold of a Hamlet or Html value than a Widget. In most usage, however, you can just stick with addWidget.

My point in explaining this now is so make clear that

    defaultLayout [$hamlet|...|]

and

    defaultLayout $ do
        addWidget [$hamlet|...|]

are equivalent. We'll be using the latter form for mostly aesthetic reasons. However, there is one time when addWidget is a requirement: due to how we implement Hamlet polymorphism, the following code won't compile:

    defaultLayout $ do
        [$hamlet|...|]
        setTitle "My title"

The type checker gets confused trying to figure out the exact type of the hamlet quasi-quotation. Using addWidget solves this problem.

<div class="advanced">
<p>Once again, there *is* no such thing as a Widget. Technically speaking, addWidget is <code>GWidget s m () -&gt; GWidget s m ()</code>, and for that matter addHamlet is <code>Hamlet (Route m) -&gt; GWidget s m ()</code>. But worrying about all these types right now is just distracting; you can just pretend you have a simple Widget datatype.</p>
<p>Oh, and what's the difference between Hamlet and Html? Html is a fully formed chunk of HTML data. Hamlet is a *function* which takes a URL renderer and returns an Html. This is how we implement type-safe URLs in Yesod: by passing around URL rendering functions.</p>
</div>

## Cassius

While Hamlet is a whitespace-sensitive templating language for HTML, Cassius is for CSS. (Don't worry, we'll get to Julius for Javascript soon.) We could rewrite the example above as:

~widgets-cassius

This produces the code (whitespace added for readability):

    <!DOCTYPE html> 
    <html>
        <head>
            <title>Hello World</title>
            <style>p{color:red}</style>
        </head>
        <body>
            <p>Hello World!</p>
        </body>
    </html>

Yesod automatically created our style tag for us. (It can also automatically create external stylesheets, but you'll need to wait till later to see that.) What's great about this approach is how composable it is: we can write entire widgets, complete with HTML and CSS, as separate functions and then simply include it in another widget. For example, if we had a footer we wanted to include on every page:

~widgets-footer

Produces the following (once again, whitespace added):

    <!DOCTYPE html> 
    <html>
        <head>
            <title>Hello World</title>
            <style>
                p {
                    color: red
                }
                #footer {
                    margin-top:10px;
                    padding-top:10px;
                    border-top:1px dashed #000;
                    text-align:center
                }
            </style>
        </head>
        <body>
            <p>Hello World!</p>
            <div id="footer">
                This page powered by the <a href="http://docs.yesodweb.com/">Yesod Web Framework</a>
            </div>
        </body>
    </html>

Notice how all of the CSS declarations get placed into the same style tag, while the HTML all gets placed in the body tag appropriately. This was a simple example of using widgets; in fact, in real code, this footer would just be included as part of your template. But these humble foundations allow very complex pages to be built up easily.

## Julius

Just for completeness, let's see a little example with Javascript.

~widgets-julius

# External File Dependencies

Enough chit-chat, let's build a real web page, complete with ridiculous Javascript and overused UI components. We'll use jQuery and jQuery UI.

~widgets-jquery

Here we've introduced the addStylesheetRemote and addScriptRemote tags and referenced the jQuery files hosted on the Google CDN. The really cool thing in this example is the generated HTML, in particular the contents of the head tag:

    <title>Hello World</title>
    <script src="http://ajax.googleapis.com/ajax/libs/jquery/1.4/jquery.min.js"></script>
    <script src="http://ajax.googleapis.com/ajax/libs/jqueryui/1.8/jquery-ui.min.js"></script>
    <link rel="stylesheet" href="http://ajax.googleapis.com/ajax/libs/jqueryui/1.8/themes/ui-lightness/jquery-ui.css">
    <style>#color-text{color:red}#color-text.green{color:green}</style>
    <script> 
    $(function(){
        $("#color-text").click(function(){
            $("#color-text").toggleClass("green");
        });
    });
     
    $(function(){
        $("button").button().click(function(){
            alert("You clicked the button.");
        });
    });
    </script>

Some things to notice:

* Even though we referenced the jquery.min.js file twice, it's only included once in our HTML. Yesod automatically filters out duplicates.

* Both Javascript snippets have been combined into a single script tag.

* The code is structured so that external script tags come before inline scripts. In many cases (such as ours), this is a necessity, since the inline Javascript references the external library.

## When a Widget is not a Hamlet

Let's take that last example and do something slightly different. Instead of simply concatenating the two widgets (colorChanger and buttons) together, we want to display that in a table (I know, **soooo** old fashioned). It turns out we can use caret-interpolation from Hamlet like so:

~widgets-caret

This is incredibly useful: we can build up very complex widgets, complete with CSS and Javascript dependencies, and then arrange their HTML however we want. We'll see that this is very important when we want to write forms. But we no longer have the ability to swap addHamlet for addWidget: the final type of a hamlet quasi-quotation must match the types of any values embedded with caret interpolation. Since colorChanger and buttons are both <code>Widget</code>s, the whole quasi-quotation is a Widget too.

<div class="advanced"><p>You can embed *any* Widget value within a quasi-quotation. In fact, you could rewrite getHomeR as:</p>

~widgets-embed

<p>Whether you would ever want to write code that way is a different story. But it's nice to know that you can.</p></div>

# Summary

Instead of having your HTML, CSS and Javascript sprawled all over the place, a Widget is a central, composable value that contains them all. This allows us to easily create reusable components that can be included in other widgets. Widgets simplify things for you further by ensuring that external files, such as jQuery, only get included once.
