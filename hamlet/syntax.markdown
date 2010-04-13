---
title: Syntax -- Hamlet
---
You might want to look at [the Haml site](http://haml-lang.com/) before reading
the rest of this, though it's certainly not required. Coming from a Haskell
background, the whitespace-sensitive layout of Hamlet should feel very
comfortable. Let's kick it off with an example:

    1 %html
    2     %head
    3         %title My Site
    4     %body
    5         %h1 $pageTitle$
    6         #wrapper
    7             Hello World!
    8         %a!href=@homeUrl@ Go home

Some of the basics should be obvious here: indentation dictates nesting. So the head and body tags are inside the html tag. Good.

Now, let's get more nitty-gritty: the percent sign means the tag name. So line 3 gets converted to:

    <title>My Site</title>

Line 6 begins with a pound; this denotes the id. However, you'll note that we haven't specified a tag name. In Hamlet (like haml), the default tag name is div. So lines 6 and 7 together generate:

    <div id="wrapper">Hello World!</div>

I haven't used it in this example, but a period will give a class name. This coincides very nicely with CSS syntax. For other attributes, we use the exclamation point. Here's a few examples of this:

    %span!style=color:red This is red
    %a!href=/some/link/ Some Link
    %input!type=checkbox!checked!name=foo

becomes

    <span style="color:red">This is red</span>
    <a href="http://www.google.com/">Google</a>
    <input type="checkbox" checked name="foo">

Which brings us very nicely to line 8. You'll notice that is says @homeUrl@; content wrapped in an @ refers to a function which returns a URL. Each template is supplied with a function which converts a URL datatype to a String. See below for a more in-depth explanation of this.

Finally, we have line 5. Instead of @, here we use $. Content wrapped in $ is a function which returns an HtmlContent. HtmlContent is simply text that specifies whether it requires entity escaping.

Now that we've seen an overview of the syntax, let's get into the details.
