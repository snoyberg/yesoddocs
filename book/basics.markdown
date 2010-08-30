---
title: Basics
---
Yesod comes with a scaffolding tool that creates a basic site template for you. This takes care of a lot of the tedious, boilerplate code you need to get started: writing a cabal file, creating default layout templates, a directory structure, etc. It's the best way to get started on a site. However, for learning Yesod, we'll start off without it. This will give you a much better feel for *how* Yesod works.

So it proper tradition, we'll start with Hello World.

    1 {-# LANGUAGE TypeFamilies, QuasiQuotes #-}
    2 import Yesod
    3 data HelloWorld = HelloWorld
    4 mkYesod "HelloWorld" [$parseRoutes|/ HomeR GET|]
    5 instance Yesod HelloWorld where approot _ = ""
    6 getHomeR = return $ RepPlain $ toContent "Hello World!"
    7 main = basicHandler 3000 HelloWorld

Let's start by looking at line 6. getHomeR is the **handler function** for the HomeR **resource pattern** when called with the GET request method. We defined the HomeR resource pattern up on line 4; the "/ HomeR GET" bit says that HomeR responds to requests on the root of our application (/) and answers the GET request method.

This is different than the approach taken by many other web frameworks. In Yesod, by default, *different request methods have different handlers*. This makes it easier to write properly RESTful web applications.

Line 6 also displays another RESTful feature: RepPlain. In REST, we often talk about **representations** of data. For example, let's say we have a Haskell datatype and value:

    data Person = Person { name :: String, age :: Int }
    michael = Person "Michael" 25

We could represent that data as HTML:

    <table>
        <tr>
            <th>Name</th>
            <td>Michael</td>
        </tr>
        <tr>
            <th>Age</th>
            <td>25</td>
        </tr>
    </table>

or we could represent it as JSON:

    {"name":"Michael","age":25}

or as XML:

    <person>
        <name>Michael</name>
        <age>25</age>
    </person>

Often times, web applications will use a different URL to get each of these representations; perhaps /person/michael.html, /person/michael.json, etc. Yesod follows the RESTful principle of a single URL for each **resource**. So in Yesod, all of these would be accessed from /person/michael/.

Then the question becomes how do we determine *which* representation to serve. The answer is the HTTP Accept header: it gives a prioritized list of content types the client is expecting. Yesod will automatically determine which representation to serve based upon this header.

Let's make that last sentence a bit more concrete with some code:

    class HasReps a where
        chooseRep :: a -> [ContentType] -> IO (ContentType, Content)

This typeclass is the core of Yesod's RESTful approach to representations. Every handler function must return an instance of HasReps. Yesod provides a number of instances of HasReps out of the box. In our example above, we saw RepPlain; this creates a response with a content-type of "text/plain". The implementation is simple:

    newtype RepPlain = RepPlain Content
    instance HasReps RepPlain where
        chooseRep (RepPlain c) _ = return ("text/plain", c)

What's interesting here is that we ignore entirely the list of expected content types. A number of the built in representations (RepHtml, RepJson, RepXml) in fact only support a single representation, and therefore what the client wants is irrelevant. An example to the contrary is RepHtmlJson. This instance helps greatly in programming AJAX applications that degrade nicely; we'll see some examples in later chapters.

<p class="advanced">The datatype Content is in fact a type synonym for the WAI's ResponseBody datatype, allowing you to create content from enumerators, static files or lazy bytestrings. More information is available in the [WAI chapter](wai.html).</p>

## Resources

I mentioned the terms resource and resource pattern above. A resource is another term for a URL. A resource pattern is a group of related URLs. A simple example of a URL pattern from above would be /name/<some name>. The individual resources /name/adam/, /name/bob/, /name/charlie/ all clearly belong to the same pattern. Let's see how we could write a program to take advantage of this:

    1 {-# LANGUAGE TypeFamilies, QuasiQuotes #-}
    2 import Yesod
    3 data Names = Names
    4 mkYesod "Names" [$parseRoutes|/name/#String NameR GET|]
    5 instance Yesod Names where approot _ = ""
    6 getNameR name = return $ RepPlain $ toContent $ "Hello " ++ name
    7 main = basicHandler 3000 Names

On line 4 we declare a resource pattern named NameR. It has two "pieces" to it: the first piece is the static piece "name", while the second is a dynamic piece matching any String. Jumping down to line 6, we see that our handler function now takes a single argument: the name passed via the URL. So if you visit "/name/Michael/", Yesod will call the function getNameR with the argument "Michael".

Another nice thing here is that you aren't limited to Strings: if you use Int instead, Yesod automatically parses the appropriate piece of the URL into an integer for you and hands it to your handler function. If the piece is not a valid integer, the user will be a 404 not found message.

## The site argument

In our two examples, we've had this extra datatype (HelloWorld in the first, Names in the second) that hasn't really seemed to be doing much.
