---
title: Yesod Book- Hamlet
---
Any templating system can be used with Yesod; as Yesod is built on top of [WAI](wai.html), we only need to be able to produce a response body understood by that interface. However, Yesod spawned the creation of a templating system called Hamlet which is designed to interoperate well with the Yesod ecosystem. It's also a very useful tool *outside* of Yesod, and is arguing the most popular of the project spun off from Yesod.

## Basics

The name Hamlet comes from its similarity to Haml. If you are familiar with the latter, you should feel right at home with Hamlet method for creating and nesting tags. However, you should know that this is a very superficial resemblance, as Hamlet approaches most things very differently than Haml.

To get our feet wet, let's give a little example:

    1 %ul#navbar
    2     %li
    3         %a!href="here.html" Here
    4     %li
    5         %a!href="there.html" There
    6 %p.red
    7     Raw Content

This shows a lot of the basics of Hamlet. Let's start with the obvious: whitespace denotes nesting. In Hamlet, *any* level of indentation indicates nesting, whether it be 1 space or 8 spaces. (By the way, a tab counts as 4 spaces, but I personally avoid tabs.) So we can see that line 3 is nested inside of line 2, line 5 within 4, 2 through 5 within 1, and 7 within 6.

Line 1 begins a ul tag; the percent sign let's you know the tag name. After the ul, there is a hash. Hamlet tries to mimick CSS declarations, where a hash is an ID and a period is a class name. Thus the first line is equivalent to &lt;ul id="navbar"&gt;. Similarly, line 6 is equivalent to &lt;p class="red"&gt;.

But let's now look at line 3; the exlamation point is used for arbitrary tag attributes. The quotes around the attribute are optional; however, if the attribute value contains a special character, you'll need the quotes. A special character would be a hash (#), period (.), exclamation point (!) or space.

Finishing line 3, we see that there is a space and then the word "Here". "Here" is now the content of the anchor tag. On line 7, we see another method for embedding content within a tag: on a new line with indentation. Both methods are equivalent.

### Default tag name

Above, we specified each tag name explicitly. However, if the tag name is excluded, it is assumed to be a div. Therefore, the following are equivalent:

    %div#nav
    #nav

or

    %div.red
    .red

or even

    %div!title="My Title"
    !title="My Title"

So far, we have seen four special characters for starting a line: percent sign, hash, period and exclamation mark. If a line starts with one of those, then the line is denoting a new tag.

### Doctype

One other convenience in Hamlet: a line consisting solely of three exclamation marks gets converted to a doctype statement. The exact doctype used is configurable, but the default is the HTML5 doctype &lt;!DOCTYPE html&gt;. Therefore, a common beginning to a Hamlet document is:

    !!!
    %html
        %head
            %title My Page title
        %body
            %h1 My Page Title

### Backslash escaping

Since Hamlet is white-space sensitive, we need a way to escape whitespace at the beginning of a line. A backslash has special meaning *when it is the first non-whitespace character*. Otherwise, it is interpretted literally as a backslash. There are three cases:

* When there are no characters following the backslash, Hamlet translates it into a newline.

* When there are two backslashes together, Hamlet translates it into a single backslash.

* Otherwise, the backslash is simply thrown out.

Let's see some examples where this is useful.

    %b Hello
    \ World

becomes

    <b>Hello</b> World

whereas

    %b Hello
    \
    World

becomes

    <b>Hello</b>
    World

One other related convenience is the trailing dollar sign. While the backslash allow leading whitespace, the dollar sign helps with trailing whitespace. The trailing dollar sign is never necessary, but it prevents overzealous text editors from pruning "extra" space characters. For example:

    Hello $
    %b World

becomes

    Hello <b>World</b>

## Calling Hamlet

So how exactly do you *use* Hamlet? Believe it or not, there are four options: quasi-quotation, template haskell, template haskell debug mode and runtime. Each method has pluses and minuses; here's a general guide to making a selection:

* If you're dealing with templates you won't have access to at all at compile time, you have to use runtime templates.

* If these are small snippets of HTML, quasi-quotation is recommended: you get to embed the Hamlet template in your source file, which simplifies the recompilation cycle.

* If you want to keep your template separate from the code, use one of the template haskell options. In general, debug mode is very convenient for development: you can see the result of most template changes without recompiling. When you're ready to move an application to production, you should switch to non-debug mode. Since both functions have the same signature, you simply have to switch the function called.

This document is not going to cover runtime templates; please see the haddocks for them. The other three are very similar. In order to create a template using quasiquotation, you could use the following Haskell code:

    [$hamlet|%h1 Hello World!|]

For the template haskell versions, you would place the template in an external file (let's say "hello.hamlet") and then write one of:

    $(hamletFile "hello.hamlet")
    $(hamletFileDebug "hello.hamlet")

In all three cases, the datatype of the resulting value would be <code>Hamlet url</code>; we will discuss what this type means in more depth later, as well as how to render a Hamlet value.

## Variables

So far, we've seen that Hamlet is a great tool for generating static HTML pages from a slightly more user-friendly syntax. But what about doing something *dynamic*, like embedding variables? We'll start with strings.

Variables in Hamlet refer to the Haskell variables in scope when the template was created. Strings are inserted into a template by surrounding them with dollar signs. So an example would be:

    let name = "Michael"
    let template = [$hamlet|Hello $name$|]

In addition, you can apply functions within variable interpolation. For example:

    let age = 25
    let template = [$hamlet|You are $show.age$ years old.|]

You can even have deeper chains:

    let person = ("Michael", 25)
    let template = [$hamlet|$fst.person$ is $show.snd.age$ years old.|]

<p class="advanced">There's also a way to allow applying a function to multiple values by using parantheses. However, this is not recommended for regular use, and will not be discussed further.</p>

### HTML Escaping

Dollar-sign interpolation on a string automatically HTML-escapes special values. For example:

    let formula = "x < y"
    [$hamlet|$formula$|]

Would in fact produce:

    x &lt; y

Let's say that you want to include raw HTML values in your templates without escaping them; in that case, you'd have to create a value of type Html; Html is a datatype provided by the blaze-html package, upon which Hamlet is built. One example would be:

    let html = preEscapedString "<b>This is bold</b>"
    [$hamlet|$html$|]

preEscapedString is a function provided by blaze-html.

### Embedding templates

In addition to inserting String and Html values, sometimes you'll want to embed another Hamlet template. For example, perhaps you want to apply a sitewide HTML template for all pages; you'll want to simply insert each page's content within a master template. In such a case, you might write Haskell code such as:

    wrap body = [$hamlet|
    !!!
    %html
        %head
            %title Some Title
        %body
            ^body^
    |]

Wrapping a variable with carets (^) allows embedding of other templates.

### URLs

Here's the part where Hamlet shines: [type-safe URLs](theory.html#type-safe-urls). Instead of splicing together strings for href and src attributes, we use at-sign interpolation:

    %a!href=@Home@ Return to homepage

You can also include query string parameters like this:

    let myurl = (Home, [("name", "Michael")])
    [$hamlet|
        %a!href=@?myurl@ Homepage with query string parameters
    |]

Notice the question mark after the at-sign.

### Conditional attributes

Now that we know about variables, we can mention one more important feature: conditional attributes. The most common use case is setting the "checked" and "selected" attributes for checkboxes and lists. An example would be:

    let isBlue = False
    let isGreen = False
    let isRed = False
    [$hamlet|
        $select!name=color
            %option!:isBlue:selected Blue
            %option!:isGreen:selected Green
            %option!:isRed:selected Red
    |]

Would produce (whitespace added for convenience):

    <select name="color">
        <option>Blue</option>
        <option selected>Green</option>
        <option>Red</option>
    </select>

The syntax is simple: if there is a colon immediately following an exclamation mark, then everything up till the next colon is taken to be a variable reference. That variable must be a Bool; if the value is True, the given attribute is set, otherwise it isn't.

## Control structures

### if/elseif/else

The syntax is very simple, and the nesting rules are a straight-forward extension of what we've seen up until now. A simple example:

    $if isOver25
        %p You are over 25 years old.
    $elseif isOver21
        %p You are over 21 years old.
    $else
        %p You're making me feel old.

### forall

forall is how we deal with lists in Hamlet. forall also introduces the concept of *variable binding*. We can use it to improve the color example from above:

    let colors =
            [ ("Blue", False)
            , ("Green", True)
            , ("Red", False)
            ]
    [$hamlet|
        $select!name=color
            $forall colors color
                %option!:snd.color:selected $fst.color$
    |]

The first parameter is the name of the list, and the second is the variable name being bound to.

### maybe/nothing

This is a very nice convenience. It works very similarly to forall, in that it introduces variable binding.

    let username = Maybe "Michael"
    [$hamlet|
        $maybe username u
            %p You are logged in as $u$.
        $nothing
            %p You are not logged in.
