---
title: Web Application Interface
---
It is a problem almost every language used for web development has dealt with: the low level interface between the web server and the application. The earliest example of a solution is the venerable and battle-worn <abbr title="Common Gateway Interface">CGI</abbr>, providing a language-agnostic interface using only standard input, standard output and environment variables.

Back when Perl was becoming the de facto web programming language, a major shortcoming of CGI became apparent: the process needed to be started anew for each request. When dealing with an interpretted language and application requiring database connection, this overhead became unbearable. FastCGI (and later SCGI) arose as a successor to CGI, but it seems that much of the programming world went in a different direction.

Each language began creating its own standard for interfacing with servers. mod_perl. mod_python. mod_php. mod_ruby. Within the same language, multiple interfaces arose. In some cases, we even had interfaces on top of interfaces. And all of this led to much duplicated effort: a Python application designed to work with FastCGI wouldn't work with mod_python; mod_python only exists for certain webservers; and this programming language specific extensions need to be written for each programming language.

Haskell has its own history. We originally had the cgi package, which provided a monadic interface. The fastcgi package then provided the same interface. Meanwhile, it seemed that the majority of Haskell web development focused on the standalone server. Theoretically, this is a great solution: we don't incur any of the overhead of having a *second* program running. In practice, these standalone servers do not support all of the features needed by a modern application, virtual hosts being the most prominent. Even if this support can be provided, you still won't get the speed offered by a well optimized server, created by a team of people focusing specifically on that goal. So in the end, the standalone servers end up using reverse HTTP proxies to sit behind more mainstream web servers.

I'm sorry to say, but this situation is a mess. Why should an application author be tied down to a certain deployment mechanism just by their choice of interface? WAI attempts to solve this, by providing a generic and efficient interface between web servers and applications. Any **handler** supporting the interface can server any WAI application, while any application using the interface can run on any handler.

At the time of writing, WAI has backends for CGI, FastCGI, SCGI, and two standalone servers: SimpleServer and Snap. This gives you a great workflow: while developing, you can use the SimpleServer and avoid any complexities involved in configuring a server. When you are ready to deploy your application, you simply switch the handler to FastCGI- or anything else- and recompile.

## The Interface

The interface itself is very straight-forward: an application takes a request and returns a response. A response is an HTTP status, a list of headers and a response body. A request contains various information: the requested path, query string, request body, HTTP version, and so on.

### Response Body

Haskell has a datatype known as a lazy bytestring. By utilizing laziness, you can creating large values without exhausting memory. Using lazy I/O, you can do such tricks as having a value which represents the entire contents of a file, yet only occupies a small memory footprint. In theory, a lazy bytestring is the only representation necessary for a response body.

In practice, while lazy byte strings are wonderful for generating "pure" values, the lazy I/O necessary to read a file introduces some non-determinism into our programs. When serving thousands of small files a second, the limiting factor is not memory, but file handles. Using lazy I/O, file handles may not be freed immediately, leading to resource exhaustion. To deal with this, WAI uses **enumerators**.

Enumerators are really a simple concept with a lot of complications surrounding them. Most basically, an enumerator is a *data producer*, that hands chunks of data one at a time to an **iteratee**, which is a *data consumer*. In the case of WAI, the request body would be an enumerator which would produce data by reading it from a file. The iteratee would be the server, which would send these chunks of data to the client.

There is one further optimization: many systems provide a sendfile system call, which sends a file directly to a socket, bypassing a lot of the memory copying inherent in more general I/O system calls. The WAI response body therefore has three constructors: one for lazy bytestrings, one for enumerators and one for files.

### Request Body

We also wish to avoid the issues of lazy I/O when reading in a request body. One approach to this would be to use enumerators. In this case, the request body would be an enumerator, and the web application would need to provide an iteratee to receive the contents. However, this would require an inversion of control in programming. Many programmers are accustomed to pulling in data, as opposed to having the data pushed to them. There are also certain cases where it could be beneficial to not process all of the data at once, and use case precluded by enumerators.

Instead, WAI specifies the request body be a <code>Source</code>. A source is a recursive datatype: it wraps an IO action that will return nothing if no data is remaining, or the next chunk of data and the next source if data was available. Using this datatype, you can pull data chunkwise from the request body.

## Hello World

To demonstrate the simplicity of WAI, let's look at a hello world example. In this example, we're going to use the OverloadedStrings language extension to avoid explicitly packing string values into bytestrings.

    1 {-# LANGUAGE OverloadedStrings #-}
    2 import Network.Wai
    3 import Network.Wai.Handler.SimpleServer
    4 import Data.ByteString.Lazy.Char8 (pack)
    5
    6 application _ = return $ Response status200 [("Content-Type", "text/plain")]
    7                        $ ResponseLBS $ pack "Hello World"
    8
    9 main = run 3000 application

Lines 2 through 4 perform our imports. SimpleServer is provided by the wai-extra package; running "cabal install wai-extra" will get you all the dependencies you need. Since overloaded strings does not help us with lazy bytestrings, we also need to pull in a pack function.

First we define our application. We are ignoring the argument to the function: this is the request information, which we are ignoring for the moment. For any request, we are returning a response with status code 200 ("OK"), and text/plain content type and a body containing the words "Hello World". Pretty straight-forward.

## Middleware

In addition to allowing our applications to run on multiple backends without code changes, the WAI allows us another benefits: middleware. Middleware is essentially an *application transformer*, taking one application and returning another one. Middlewares can be used to provide lots of services: cleaning up URLs, authentication, caching, JSON-P requests. But perhaps the most useful and most intuitive middleware is gzip compression. The middleware works very simply: it parses the request headers to determine if a client supports compression, and if so compresses the response body and adds the appropriate response header.

The great thing about middlewares is that they are unobtrusive. Let's see how we would apply the gzip middleware to our hello world application.

    1  {-# LANGUAGE OverloadedStrings #-}
    2  import Network.Wai
    3  import Network.Wai.Handler.SimpleServer
    4  import Network.Wai.Middleware.Gzip
    5  import Data.ByteString.Lazy.Char8 (pack)
    6
    7  application _ = return $ Response status200 [("Content-Type", "text/plain")]
    8                         $ ResponseLBS $ pack "Hello World"
    9
    10 main = run 3000 $ gzip application

We added an import line to actually have access to the middleware, and then simply applied gzip to our application. You can also *chain together* multiple middlewares: a line such as <code>gzip $ jsonp $ othermiddleware $ myapplication</code> is perfectly valid. One word of warning: the order the middleware is applied can be important. For example, jsonp needs to work on uncompressed data, so if you apply it after you apply gzip, you'll have trouble.
