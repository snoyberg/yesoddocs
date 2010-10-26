# Getting Yesod

The rest of this book will assume you have Yesod installed. You'll need to:

* Install the [Haskell Platform](http://hackage.haskell.org/platform/)
* Run <code>cabal update</code>
* Run <code>cabal install yesod</code>

You will also likely want to run <code>cabal install yesod-auth</code>.

# Library versus Framework

I'm going to be a bit bold and say the defining line between a library and a framework is that a framework tells you how to lay out your code into a file/folder structure. You may not agree with this definition, but it's useful to explain how this book will begin.

The Yesod Web Framework comes with a tool that automatically generates a full site template with a bunch of bells and whistles. This is the recommended way to get started on a new Yesod application. This added convenience, however, hides away some of the important details going on behind the scenes.

So to start off, we're going to be treating Yesod as a library. Having to explicitly write all the code is a good exercise to get started. [Later on](/book/scaffold/), we'll introduce the scaffolding tool and describe the standard layout of a Yesod project.

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

The only truly new feature is inside the hamlet quasi-quotation on lines 10-12. We'll delve into syntax later, but we can see that:

    %a!href=@Page1R@ Go to page 1!

creates a link to the Page1R resource. The important thing to note here is that Page1R is a data constructor. By making each resource a data constructor, we have a feature called **type-safe URLs**. Instead of splicing together strings to create URLs, we simply create a plain old Haskell value. By wrapping a type-safe URL in at-signs (@) in Hamlet, Yesod automatically renders those values to textual URLs before sending things off to the user.

# Development server

One of the advantages interpreted languages have over compiled languages is fast prototyping: you save changes to a file and hit refresh. If we want to make any changes to our Yesod apps above, we'll need to call runhaskell from scratch, which can be a bit tedious.

Fortunately, there's a nice solution to this: [wai-handler-devel](http://hackage.haskell.org/package/wai-handler-devel-0.1.0.1) embeds a Haskell interpreter and automatically reloads code changes for you. This can be a great way to develop your Yesod projects, and when you're ready to to move to production, you can compile against a more efficient backend. The Yesod site template comes built in with a script to do this for you.

It's a little bit more involved to set up your code to be used by wai-handler-devel, so our examples will just use basicHandler. But as a simple example, try saving the following as HelloWorld.hs:

~basics-devel

Make sure you have wai-handler-devel installed:

    cabal install alex
    cabal install happy
    cabal install wai-handler-devel

And then run your code with:

    wai-handler-devel 3000 HelloWorld withHelloWorld

This will run a development server on port 3000, using module HelloWorld and the function withHelloWorld. Try making changes to the HelloWorld.hs file and reloading: they should show up automatically. In order to shut down the server, type "q" and hit enter on the command line.

# Summary

Every Yesod application is built around a foundation datatype. We associate some resources with said datatype and define some handler functions, and Yesod handles all of the routing. These resources are also data constructors, which lets us have type-safe URLs.

By being built on top of WAI, Yesod applications can run with a number of different backends. basicHandler is an easy way to get started, as it's included with Yesod. For rapid development, wai-handler-devel is a good choice. And when you're ready to move to production, there are high-performance options like FastCGI and Snap.
