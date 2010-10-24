# Introduction

One of the challenges in web development is that we have to coordinate three different client-side technologies: HTML, CSS and Javascript. Worse still, we have to place these components in different locations on the page: CSS in a style tag in the head, Javascript in a script tag in the head, and HTML in the body. And never mind if you want to put your CSS and Javascript in separate files!

In practice, this works out fairly nicely when building a single page, becuase we can separate our structure (HTML), styling (CSS) and logic (Javascript). But when we want to build modular pieces of code that can be easily composed, it can be a headache to coordinate all three pieces separately. Widgets are Yesod's solution to the problem. They also help with the issue of including libraries, such as jQuery, one time only.

# Expanding Hello World

In the previous chapter, our Hello World example produced some very basic HTML:

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

<p class="advanced">You may be scratching your head wondering what's going on the sudden addition of addBody. It turns out that a Hamlet template is *polymorphic*; it can be a plain Html value, a Hamlet value, or even a widget. When we treat it as a Hamlet, we need to use the addBody function to insert it into a widget.</p>

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

# FIXME: Using caret interpolation

# Summary

Instead of having your HTML, CSS and Javascript sprawled all over the place, a Widget is a central, composable value that contains them all. This allows us to easily create reusable components that can be included in other widgets. Widgets simplify things for you further by ensuring that external files, such as jQuery, only get included once.
