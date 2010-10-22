## FIXME Refactor

New structure:

* Framework versus library (scaffolding), use library to learn
* Hello World
* Routing, handler functions
* Site argument datatype
* WAI application, any handler
* Routes datatype, resources, type safe URLs
* Touch on Hamlet, mention cassius and julius
* Example with links
* wai-handler-devel

<hr>

# Library versus Framework

I'm going to be a bit bold a say the defining line between a library and a framework is that a framework tells you how to lay out your code into a file/folder structure. You may not agree with this definition, but it's useful to explain how this book will start off.

The Yesod Web Framework comes with a tool that automatically generates a full site templates with a bunch of bells and whistles. This is the recommended way to get started on a new Yesod application. This added convenience, however, hides away some of the important details going on under the scenes.

So to start off, we're going to be treating Yesod as a library. Having to explicitly write all the code is a good exercise to get started. Later on, we'll introduce the scaffolding tool and describe the standard layout of a Yesod project.

# Hello World

Let's get this book started properly: a simple web page that says Hello World:

~basics-helloworld

If you save that code in <code>helloworld.hs</code> and run it with <code>runhaskell helloworld.hs</code>, you'll get a web server running on port 3000. If you point your browser to [http://localhost:3000](http://localhost:3000), you'll get the following HTML:

    <!DOCTYPE html> 
    <html><head><title></title></head><body>Hello World!</body></html>

## Routing

Like most modern web frameworks, Yesod follows a [front controller pattern](http://en.wikipedia.org/wiki/Front_Controller_pattern). This means that every request to a Yesod application enters at the same point and is routed from there. As a contrast, in systems like PHP and ASP you often times create a number of different files, and the web server automatically directs requests to the relevant file.

Lines 4 through 6 set up this routing system. We see our only **resource** defined on line 5. We'll give full details of the syntax later, but this line creates a resource named HomeR, which accepts GET requests at the root (/) of our application.

Yesod sees this resource declaration, and determines to call the getHomeR **handler function** whenever it receives a request for HomeR. The function name follows the simple pattern of request method, in lowercase, followed by the resource name.

## Handler function

Most of the code you write in Yesod lives in handler functions. This is where you process user input, perform database queries and create responses. In our simple example, we create a response using the defaultLayout function. By default, this is a simply HTML wrapper that creates a doctype, html, head and body tags. [As we'll see later](/book/yesod-typeclass/), this function can be overridden to do much more.

That funny <code>[$hamlet|Hello World!|]</code> is a **quasi-quotation**. It allows us to embed arbitrary text in our Haskell code, process it with a specific function and have that generate Haskell code, all at compile time. In our case, we feed the string "Hello World!" to the hamlet quasi-quoter.

Hamlet is the default HTML templating engine in Yesod. Together with its siblings Cassius and Julius, you can create HTML, CSS and Javascript in a fully type-safe and compile-time-checked manner. We'll see much more about this [when we discuss widgets](/book/widgets/).

## The Foundation

The word "HelloWorld" shows up on lines 3, 4, 7 and 9, yet the datatype doesn't seem to actually do anything important. In fact, this seemingly irrelevant piece of code is central to how Yesod works. Each Yesod application has a single datatype, referred to as its **foundation**.

Line 3 of our example defines this simple datatype. Line 4 does something a bit more interesting: it associates the routing rule we define on line 5 with this datatype. Each foundation *must* be an instance of the Yesod typeclass; we do this on line 7. (We'll get into much more detail on the Yesod typeclass and the approot method in [the next chapter](/book/yesod-typeclass/).

<p class="advanced">By the way, the word Yesod (יסוד) means *foundation* in Hebrew.</p>

## Running

Once again we mention HelloWorld in our main function. Our foundation contains all the information we need to route and respond to requests in our application, now we just need to convert it into something that can run. A great function for this in Yesod is basicHandler, which runs a simple web server on the port specified (here, it's 3000).

<p class="advanced">Technically speaking, basicHandler does a little bit more than that. It checks to see if there are any CGI environment variables present. If so, it runs as CGI. Otherwise, as a standalone server.</p>

One of the great features of Yesod is that you aren't tied down to a single deployment strategy. Yesod is built on top of the Web Application Interface (WAI), allowing it to run on FastCGI, SCGI, the Snap web server, or even as a desktop application using the Webkit library. We'll discuss some of these options in the [deployment chapter](/book/deploying/). And at the end of this chapter, will explain the [development server](#wai-handler-devel).

# Resources and type-safe URLs

In our hello world, we defined just a single resource (HomeR). A web application is usually much more exciting with more than one page on it. Let's take a look:

~basics-links

Overall, this is the same. Our foundation is now Links instead of HelloWorld, and in addition to the HomeR resource, we've added Page1R and Page2R. As such, we've also added two more handler functions: getPage1R and getPage2R.

The only truly new feature is inside the hamlet quasi-quotation on lines 10-12. We'll delve into syntax later, but

<hr>

# FIXME Old stuff

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

I mentioned the terms resource and resource pattern above. A resource is another term for a URL. A resource pattern is a group of related URLs. A simple example of a URL pattern from above would be /name/*some-name*/. The individual resources /name/adam/, /name/bob/, /name/charlie/ all clearly belong to the same pattern. Let's see how we could write a program to take advantage of this:

FIXME ~basics-names

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

This piece of code defines three resource patterns: RootR, NameR and AgeR. We've already seen how this interacts with the dispatch system: we will need to write handler functions getRootR, getNameR and getAgeR. What we **haven't** seen is that this also creates a data type:

    data MyAppRoute = RootR | NameR String | AgeR Int

As simple as this looks, it's one of the most powerful features Yesod offers for ensuring the safety of your applications. Every valid URL in our application can be expressed as a value of type MyAppRoute. Here are some mappings:

    /             -> RootR
    /name/michael -> NameR "michael"
    /age/935      -> AgeR 935

In Yesod, we don't splice together strings to generate URLs; we simply build up plain old Haskell data types. This means **the compiler can prevent invalid links**. This feature ties in very nicely with Yesod's templating system (Hamlet), as [we'll see later](../templates/).

As I mentioned, there is an associated type (type family) involved as well. This looks like:

    type instance Route MyApp = MyAppRoute

Understanding type families is not a prerequisite to using Yesod, though you may find it useful. Type families also play a big role in our persistence layer.

## Summary

This chapter set out some of the foundations of Yesod. We stay very close to RESTful principles by having a unique resources (URLs) and allowing multiple representations. We also respect that different request methods mean different things, and use different handlers by default.

Every Yesod application has a site argument datatype, which is central to everything it does. There is also an associated type-safe URL datatype which gives Yesod very strong type safety guarantees when constructing links.
