---
title: Yesod Book- Deploying Your Webapp
---
<blockquote><b>Dammit, I'm a programmer, not a sysadmin!</b></blockquote>

You've now written the most perfect web application ever. It makes angels sing and babies laugh. Now comes the most dreaded part of the process: deployment. You must somehow take your pure code and throw it out into the unholy realm of the Interwebs. What ever shall you do?

Yesod tries to give you as much flexibility at this point as possible. Some people have their own dedicated servers and want to run their Yesod app as a standalone application. Some people are stuck on shared hosting and want to use CGI. All of these are supported as first class citizens in the Yesod world.

This magic is provided by the WAI; Yesod targets an abstract interface that is supported by multiple handlers. You can [read more about WAI](wai.html), but the point here is to set up an optimal serving strategy.

## Development

During development, you don't want to fuss around with multiple servers, HTTP accelerators, caching, or even having a separate web server! You want to just run your application. For this, the SimpleServer handler is the most appropriate. Yesod comes built in with a function called basicHandler that calls SimpleServer for you.

<p class="advanced">basicHandler actually checks the environment to see if it is being called as a CGI script and then acts appropriately. This can be convenient for low-end hosting setups, but isn't really applicable for most users.</p>

SimpleServer is completely unoptimized, so it's not really appropriate for serving large sites. That said, small, personal applications may find it useful. For example, the [hledger](http://hackage.haskell.org/package/hledger) program uses SimpleServer as its default handler.

Let's see this in action: first we'll set up a typical Hello World application:

    {-# LANGUAGE TypeFamilies, QuasiQuotes, TemplateHaskell #-}
    import Yesod
    data HelloWorld = HelloWorld
    mkYesod "HelloWorld" [$parseRoutes|/ Home GET|]
    instance Yesod HelloWorld where approot _ = ""
    getHome = return $ RepPlain $ toContent "Hello World!"

We now need to set up our main function to call the correct handler. We'll need to import the handler at the beginning of our program:

    import Network.Wai.Handler.SimpleServer

Then we need to write a main function like:

    main = toWaiApp HelloWorld >>= run 3000

toWaiApp provides the gateway to the WAI world. run is the simple server, and 3000 is the port it runs on.

### It's a Snap

Just because it's so easy to demonstrate, let's talk about Snap. Snap Framework is another web framework in Haskell; one of the first components they have released is a web server. I won't talk benchmarks here, you should see [their site](http://snapframework.com/) for details, but it's fast. The great thing for Yesod is that it has the *exact same interface* as SimpleServer; all you have to do is replace the import line with

    import Network.Wai.Handler.Snap

### Let's go retro: desktop apps

There's one extra nifty handler that allows you to convert a web app into a desktop app: wai-handler-webkit. It simply runs SimpleServer and then starts a Webkit window to connect to it. To use it, you just do:

    import Network.Wai.Handler.Webkit
    main = toWaiApp HelloWorld >>= run "Titlebar Text"

## Production Servers

Now, with those little details out of the way, let's get down to business: how do you server a production web application? Clearly, this document can't discuss every method available, so instead I'll describe *my* method. I use a separate web server and have it communicate with my Yesod apps via FastCGI. I also make sure that all of my static files are served from a separate subdomain by the web server itself.

Obviously, the config files are very server-specific. The documentation site provides a number of examples, but here we'll only be discussing [nginx](http://nginx.org/). It's generally considered to be one of the fastest web servers out there and has a very sane config file format. Some of the options are a bit confusing, and you have to spawn your own FastCGI processes, but overall this proves to be a very stable setup.

### FastCGI

Let's take a little detour and discuss FastCGI itself; having a basic understanding of how it works will make the next steps much easier to grasp. A FastCGI application communicates with the web server over a **socket**. It accepts connection from this socket, speaks a FastCGI-specific language with the server to get the request headers and body, and then sends the response headers and body back over that socket.

Web servers like Apache and Lighttpd hide that whole socket piece from you; they let you pretend that FastCGI is almost identical to CGI. They do this by automatically spawning the process for you and binding it to a specific socket. nginx **does not spawn or bind** for you; you must do this manually. To accomplish this, we're going to use a tool called [spawn-fcgi](http://redmine.lighttpd.net/projects/spawn-fcgi).

The first question is: what socket should they communicate over? You can use TCP sockets and listen on a specific port, but I prefer using [Unix named sockets](http://en.wikipedia.org/wiki/Unix_domain_socket). Let's say that I have a Yesod FastCGI executable located at /myapp/fastcgi and I want to communicate using the socket /myapp/socket; I would run the command:

    spawn-fcgi -n -u michael -s /myapp/socket -- /myapp/fastcgi

The "-n" tells spawn-fcgi not to fork into the background, and "-u michael" says to run as user "michael". I could also include options to the Yesod application; to make it use two system threads for execution, I'd write:

    spawn-fcgi -n -u michael -s /myapp/socket -- /myapp/fastcgi +RTS -N2

Another option is to run **multiple FastCGI processes**. To do that, you're going to need another tool: [multiwatch](http://redmine.lighttpd.net/projects/multiwatch). You just add it in like this:

    spawn-fcgi -n -u michael -s /myapp/socket -- multiwatch -f 3 /myapp/fastcgi +RTS -N2

"-f 3" says to fork three separate processes. We'll discuss later on how to ensure your process keeps running.

### Configuring nginx

Before I get started, let me make one thing clear: *always use absolute paths with nginx*. Anyway, let's build this up slowly, by starting with a simple file server:

    daemon off;

    events {
    }

    http {
        server {
            listen      8080;
            root        /home/michael;
        }
    }

"daemon off;" prevents the server from forking into the background. The "events" block is required in all config files; you can see more options for it [on the nginx wiki](http://wiki.nginx.org/NginxHttpEventsModule). The http section is the important bit. You can set up multiple "server" blocks within. Here, we give two directives for our server: listen on port 8080, and serve files from /home/michael.

Let's take the next leap and serve a FastCGI application. Firstly, there is some generic configuration that needs to go into every nginx config file. In nginx, you have to explicitly include a number of the FastCGI variables. The following list should provide everything Yesod needs:

    fastcgi_param  QUERY_STRING       $query_string;
    fastcgi_param  REQUEST_METHOD     $request_method;
    fastcgi_param  CONTENT_TYPE       $content_type;
    fastcgi_param  CONTENT_LENGTH     $content_length;
    fastcgi_param  PATH_INFO          $fastcgi_script_name;
    fastcgi_param  SERVER_PROTOCOL    $server_protocol;
    fastcgi_param  GATEWAY_INTERFACE  CGI/1.1;
    fastcgi_param  SERVER_SOFTWARE    nginx/$nginx_version;
    fastcgi_param  REMOTE_ADDR        $remote_addr;
    fastcgi_param  SERVER_ADDR        $server_addr;
    fastcgi_param  SERVER_PORT        $server_port;
    fastcgi_param  SERVER_NAME        $server_name;

Next, let's convert that server block above to talk to our FastCGI application mentioned above. Remember, that application listened on the socket "/myapp/socket"; our server block therefore is:

    server {
        listen       8080;

        location / {
            fastcgi_pass   unix:/myapp/socket;
        }
    }

We're saying we want to serve the root of our domain with the FastCGI application listening on the Unix socket /myapp/socket. Once you get used to this, it's actually a fairly nice syntax.

### Virtual hosts

Virtual hosts allow us to serve different content from different domain names. So let's start getting serious: suppose that our domain name is yesodweb.com. We want to serve our app from www.yesodweb.com and serve our static content from static.yesodweb.com. Assuming the static files are located at /myapp/static:

    server {
        listen       8080;
        server_name  www.yesodweb.com;


        location / {
            fastcgi_pass   unix:/myapp/socket;
        }
    }
    
    server {
        listen       8080;
        server_name  static.yesodweb.com;
        root         /myapp/static;
    }

And one other little enhancement: if someone goes to yesodweb.com (without the www) they should be automatically forwarded to the correct URL:

    server {
        listen       8080;
        server_name  yesodweb.com;
        rewrite ^/(.*) http://www.yesodweb.com/$1 permanent;
    }

### Mime types

One little detail I skipped above is mime types: you need to provide nginx with a extension-to-mime-type mapping. nginx comes with a file mime.types by default; you can simply copy the mime.types file to the same folder as nginx.conf and insert the following inside your http block:

    include mime.types;

If you want to enter them manually, the format looks like this:

    types {
        text/html    html htm;
        text/css     css;
        image/jpeg   jpeg jpg;
    }

## Optional: port forwarding

You'll notice that I've used port 8080 throughout this example. If you want, you can simply switch the file to use port 80 and then run nginx as root. One of the advantages of starting the FastCGI process separately is you don't need be to concerned about the permissions assigned to your app.

Another approach is to leave your web server running on an inprivileged port and then use port forwarding to redirect incoming traffic from port 80 to your port. For example, on Linux you could write iptables rules like this:

    iptables -t nat -A PREROUTING -p tcp --dport 80 -j REDIRECT --to 8080

## Keep it running

So we now have two processes to initiate and monitor: nginx and the Yesod FastCGI app. There are lots of options here: distribution-specific tools like Upstart, running them from a screen session, init scripts. My tool of choice is [daemontools](http://cr.yp.to/daemontools.html); for each process you want monitored, you keep a separate directory under /service, and put a "run" script in each folder. I might therefore have the following files:

    # /service/nginx/run
    exec nginx -c /myapp/nginx.conf
    # or I could force the server to run as a different user instead
    exec setuidgid webserver nginx -c /myapp/nginx.conf
    
    # /service/myapp/run
    exec spawn-fcgi -n -u michael -s /myapp/socket -- /myapp/fastcgi
