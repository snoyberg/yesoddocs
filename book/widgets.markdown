A web page is made up of more than HTML. Specifically, Javascript and CSS play a very important role. Let's create a little page to demonstrate the three technologies working together:

    !!!
    %html
        %head
            %title CSS and Javascript Demo
            %script!src=@StaticR.jquery_js@
            %script!src=@StaticR.demo_js@
            %link!rel=stylesheet!href=@StaticR.demo_css@
        %body
            %h1 Hello World

In our static folder, we would place jquery.js from a standard jQuery archive, and then the following two files:

    /* demo.js */
    jQuery("h1").click(function(){
        alert("Hello");
    });

    /* demo.css */
    h1 {
        color: red;
    }

One of the great advantages of the HTML/CSS/JS split is that we get to split up our structure, presentation and logic into separate places. However, it makes it very difficult to compose things together. For example, let's say that we already have an HTML template file that looks like this:

    !!!
    %html
        %head
            %title Some Title
            %link!rel=feed...
        %body
            #content-wrapper
                ^content^

Our template calls a sub-template called "content", which is how we would embed our per-page content. But we have a problem: we need to place our Javascript and CSS references in the head, not in the body.

## PageContent

Yesod defines a datatype PageContent which keeps the title, head and body content of a page. To get our previous template to work with PageContent, we would change it to:

    !!!
    %html
        %head
            %title $pageTitle.pc$
            %link!rel=feed...
            ^pageHead.pc^
        %body
            #content-wrapper
                ^pageBody.pc^

This neatly solves the issue. Now our above example would need to be converted to a value of type PageContent:

    let pc = PageContent
            { pageTitle = "CSS and Javascript Demo"
            , pageHead = [$hamlet|
                %script!src=@StaticR.jquery_js@
                %script!src=@StaticR.demo_js@
                %link!rel=stylesheet!href=@StaticR.demo_css@
            |]
            , pageBody = [$hamlet|
                %h1 Hello World
            |]
            }

This separation of the head and body of an HTML document is helpful, but not a panacea. Suppose in addition to our little h1 tag, we want to have an h2 tag as well that also responds to clicks. It would also like to use jQuery for registering events, and add some CSS for styling. This might end up being a PageContent that looks like:

    let pc = PageContent
            { pageTitle = "CSS and Javascript Demo #2"
            , pageHead = [$hamlet|
                %script!src=@StaticR.jquery_js@
                %script!src=@StaticR.demo2_js@
                %link!rel=stylesheet!href=@StaticR.demo2_css@
            |]
            , pageBody = [$hamlet|
                %h2 Goodbye World
            |]
            }

At first, this doesn't seem too bad: simply combine the heads and bodies, and pick one of the titles to be the title of the page. Unfortunately, this has some shortcomings:

* We will end up with the jquery library referenced twice. This simply bloats our HTML file.

* There's no way to combine the CSS and Javascript code into a single file. Instead, if we continue with this approach, we will have dozens of small files downloaded for each page request, an untenable proposition.

## Widgets

Widgets are actually a very simple concept, probably the simplest concept to implement in Yesod. Technically, it's a monad transformer stack with a bunch of different writers. We have a separate writer for Javascript files and code, CSS files and code, page title, page head and page body. There's also a state monad for integers; we'll get to that later.

You might be wondering what I meant by CSS **files** and CSS **code**. A file means an external URL that will be referenced by this page through a link tag. Code is raw CSS code your Yesod application can generate. In the simplest form, this code will simply be injected in a style tag, though we'll see more about this later. There are a few advantages to the latter approach:

* You get to use the Cassius templating system, which provides a slight simplification of CSS syntax, generates very compressed CSS and allows variables interpolation.

* You can generate small snippets of CSS throughout your code and Yesod will automatically concatenate it into a single block.
