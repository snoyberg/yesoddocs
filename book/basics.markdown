Yesod comes with a scaffolding tool that creates a basic site template for you. This takes care of a lot of the tedious, boilerplate code which you need to get started: writing a cabal file, creating default layout templates, a directory structure, et cetera. It's the best way to get started on a site. However, for learning Yesod, we'll start off without it. This will give you a much better feel for *how* Yesod works.

So in proper tradition, we'll start with Hello World.

~basics-helloworld

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
        chooseRep (RepPlain content) _ = return ("text/plain", content)

What's interesting here is that we ignore entirely the list of expected content types. A number of the built in representations (RepHtml, RepJson, RepXml) in fact only support a single representation, and therefore what the client wants is irrelevant. An example to the contrary is RepHtmlJson, which provides either an HTML or JSON representation. This instance helps greatly in programming AJAX applications that degrade nicely; we'll see some examples in later chapters.

<p class="advanced">The datatype Content is in fact a type synonym for the WAI's ResponseBody datatype, allowing you to create content from enumerators, static files, or lazy bytestrings. More information is available in the [WAI chapter](wai.html).</p>

## Resources

I mentioned the terms resource and resource pattern above. A resource is another term for a URL. A resource pattern is a group of related URLs. A simple example of a URL pattern from above would be /name/<some name>. The individual resources /name/adam/, /name/bob/, /name/charlie/ all clearly belong to the same pattern. Let's see how we could write a program to take advantage of this:

~basics-names

On line 4 we declare a resource pattern named NameR. It has two "pieces" to it: the first is a static piece "name", while the second is a dynamic piece matching any String. Jumping down to line 6, we see that our handler function now takes a single argument: the name passed via the URL. So if you visit "/name/Michael/", Yesod will call the function getNameR with the argument "Michael".

Another nice thing here is that you aren't limited to Strings: if you use Int instead, Yesod automatically parses the appropriate piece of the URL into an integer for you and hands it to your handler function. If the piece is not a valid integer, the user will be served a 404 not found message.

## The site argument

In our two examples, we've had this extra datatype (HelloWorld in the first, Names in the second) that hasn't really seemed to be doing much. We call this the **site argument**, and contrary to appearances it is central to a Yesod application. Its purposes can be broken down into three groups:

* Storing data loaded at the initialization of your webapp. This could be settings loaded from a file, database connections, static file contents loaded into memory, etc.

* Instanciation of various type classes. You've seen one example so far: the Yesod type class. You'll see a few more of these typeclasses in the future. For the most part, they provide a method for you to supply configuration to Yesod about your application.

* Type safe URLs are a central concept to Yesod, and we access this via **associated types** (aka, type families).

The last line of our hello world example was:

    main = basicHandler 3000 HelloWorld

basicHandler is a function that runs a Yesod application using a simple HTTP server, and 3000 is the port it listens on. In this case, HelloWorld required no initialization, so we could load it up like this. However, we could also use any IO action necessary to construct the HelloWorld value before entering the application.

## Type safe URLs

And let's finally pull things full circle. We've spoken about resource patterns, and each time there was some name associated with a resource pattern. Let's put together a snippet of an application to demonstrate:

    1 data MyApp = TypeSafe
    2 mkYesod "MyApp" [$parseRoutes|
    3 /             RootR GET
    4 /name/#String NameR GET
    5 /age/#Int     AgeR  GET
    6 |]

This is how we would create a new application with a site argument called MyApp and three resource patterns. If this style of Haskell looks unfamiliar to you, it's a combination of quasi-quotation and template haskell. You don't need to understand the details to use Yesod, but it is essentially letting us create a brand new language within Haskell to allow us to write concise, safe code.

This piece of code defines three resource patterns: RootR, NameR and AgeR. We've already seen that this interacts with the dispatch system: we will need to write handler functions getRootR, getNameR and getAgeR. What we **haven't** seen is that this also creates a data type:

    data MyAppRoute = RootR | NameR String | AgeR Int

As simple as this looks, it's one of the most powerful features Yesod offers for ensuring the safety of your applications. Every valid URL in our application can be expressed as a value of type MyAppRoute. Here are some mappings:

    /             -> RootR
    /name/michael -> NameR "michael"
    /age/935      -> AgeR 935

In Yesod, we don't splice together strings to generate URLs; we simply build up plain old Haskell data types. This means **the compiler can prevent invalid links**. This feature ties in very nicely with Yesod's templating system (Hamlet), as we'll see later.

As I mentioned, there is an associated type (type family) involved as well. This looks like:

    type instance Route MyApp = MyAppRoute

Understanding type families is not a prerequisite to using Yesod, though you may find it useful. Type families also play a big role in our persistence layer.

## Summary

This chapter set out some of the foundations of Yesod. We stay very close to RESTful principles by having a unique resources (URLs) and allowing multiple representations. We also respect that different request methods mean different things, and use different handlers by default.

Every Yesod application has a site argument datatype, which is central to everything it does. There is also an associated type-safe URL datatype which gives Yesod very strong type safety guarantees when constructing links.
