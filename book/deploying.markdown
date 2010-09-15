<blockquote><b>Dammit, I'm a programmer, not a sysadmin!</b></blockquote>

You've now written the most perfect web application ever. It makes angels sing and babies laugh. Now comes the most dreaded part of the process: deployment. You must somehow take your pure code and throw it out into the unholy realm of the Interwebs. What ever shall you do?

Yesod tries to give you as much flexibility at this point as possible. Some people have their own dedicated servers and want to run their Yesod app as a standalone application. Some people are stuck on shared hosting and want to use CGI. All of these are supported as first class citizens in the Yesod world.

This magic is provided by the WAI; Yesod targets an abstract interface that is supported by multiple handlers. You can [read more about WAI](/book/wai/), but the point here is to set up an optimal serving strategy.

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

## Other means

NOTE: This needs to be cleaned up. It has some sample config files for deployment.

Since Yesod is built on top of the wai package, you access to any WAI handler when serving your application. At this time, I am aware of the following options:

* The simple-server handler in the wai-extra package.

* The CGI handler in the wai-extra package.

* The wai-handler-fastcgi package.

* There is experimental support for WAI in the happstack-server darcs repo. The will hopefully be released soon.

* Yesod also comes built-in with the basicHandler, which is simply a wrapper around simple-server and the CGI handler.

You most likely do not want to use simple-server in production; it was written for usage in testing. Its performance is most likely abysmal. For standalone settings, the happstack-server is very promising.

This document will cover the CGI and FastCGI handlers, both since these are commonly supported and because they can be tricky to set up. The first step is simply to compile your application with the appropriate handler.

Once you have the executable, you'll need to do some configuration. This can often times be the most frustrating part of web development (it was for me). I'll put some sample configuration files that I've tested in production below. If you have more samples for different server/backend configurations, please let me know!

### CGI on Apache

CGI and FastCGI work almost identically on Apache, so it should be fairly straight-forward to port this configuration. You essentially need to accomplish two goals:

1) Get the server to serve your file as (Fast)CGI.

2) Rewrite all requests to your site to go through the (Fast)CGI executable.

Here is the configuration file for [my blog](http://www.snoyman.com/blog/), with an executable named "bloggy.cgi", living in a subfolder named "blog" of the document root. "/f5/snoyman/public/blog" is the full path to the folder holding bloggy.cgi. This runs on [NearlyFreeSpeech.net](http://www.nearlyfreespeech.net).

    Options +ExecCGI
    AddHandler cgi-script .cgi
    Options +FollowSymlinks

    RewriteEngine On
    RewriteRule ^/f5/snoyman/public/blog$ /blog/ [R=301,S=1]
    RewriteCond $1 !^bloggy.cgi
    RewriteCond $1 !^static/
    RewriteRule ^(.*) bloggy.cgi/$1 [L]

The first RewriteRule is to deal with subfolders. In particular, it redirects a request for "http://www.snoyman.com/blog" to "http://www.snoyman.com/blog/". The first RewriteCond prevents directly requesting the executable, the second allows Apache to serve the static files, and the last line does the actual rewriting.

### FastCGI on lighttpd

For this example, I've left off some of the basic FastCGI settings like mime-types. I also have a more complex file in production that prepends "www." when absent and serves static files from a separate domain. However, this should serve to show the basics.

Here, "/home/michael/fastcgi" is the fastcgi application. The idea is to rewrite all requests to start with "/app", and then serve everything beginning with "/app" via the FastCGI executable.

    server.port = 3000
    server.document-root = "/home/michael"
    server.modules = ("mod_fastcgi", "mod_rewrite")

    url.rewrite-once = (
      "(.*)" => "/app/$1"
    )

    fastcgi.server = (
        "/app" => ((
            "socket" => "/tmp/test.fastcgi.socket",
            "check-local" => "disable",
            "bin-path" => "/home/michael/fastcgi", # full path to executable
            "min-procs" => 1,
            "max-procs" => 30,
            "idle-timeout" => 30
        ))
    )

### CGI on lighttpd

This is basically the same as the FastCGI version, but tells lighttpd to run a file ending in ".cgi" as a CGI executable. In this case, the file lives at "/home/michael/myapp.cgi".

    server.port = 3000
    server.document-root = "/home/michael"
    server.modules = ("mod_cgi", "mod_rewrite")

    url.rewrite-once = (
        "(.*)" => "/myapp.cgi/$1"
    )

    cgi.assign = (".cgi" => "")

### FastCGI on nginx

nginx treats FastCGI a little bit differently than Apache and lighttpd. In nginx, the user is responsible for starting the FastCGI process, **not** the web server. Without getting into too much detail, this means you need to run your FastCGI executable with a wrapper program, the best example being [spawn-fcgi](http://redmine.lighttpd.net/projects/spawn-fcgi).

There are lots of options available here; you can choose between having your application receive requests via TCP or Unix sockets, port number, etc. See the spawn-fcgi docs for more details. Here's the command I use in the [yesod-hello](http://github.com/snoyberg/yesod-hello) repository:

    spawn-fcgi -n -s /tmp/hello.socket hello

The -n option says not to run the program in the background; "-s /tmp/hello.socket" says to use a Unix named socket; and "hello" is the name of the executable.

Next comes the nginx config file. Please note that I have *not* mastered nginx config files, and suggestions on improving this are welcome.

    error_log  /dev/null;
    pid        /tmp/nginx.pid;
    daemon off;
     
    events {
      worker_connections  4096;
    }
     
    http {
      fastcgi_param  QUERY_STRING       $query_string;
      fastcgi_param  REQUEST_METHOD     $request_method;
      fastcgi_param  CONTENT_TYPE       $content_type;
      fastcgi_param  CONTENT_LENGTH     $content_length;
      fastcgi_param  PATH_INFO          $request_uri;
      fastcgi_param  SERVER_PROTOCOL    $server_protocol;
      fastcgi_param  GATEWAY_INTERFACE  CGI/1.1;
      fastcgi_param  SERVER_SOFTWARE    nginx/$nginx_version;
      fastcgi_param  REMOTE_ADDR        $remote_addr;
      fastcgi_param  SERVER_ADDR        $server_addr;
      fastcgi_param  SERVER_PORT        $server_port;
      fastcgi_param  SERVER_NAME        $server_name;
     
      access_log off;
      server_names_hash_bucket_size 128; # this seems to be required for some vhosts
     
      server {
        listen       3000;
     
        location ~ $ {
          fastcgi_pass   unix:/tmp/hello.socket;
        }
      }
    }

Things of note: the "daemon off" line says not to fork into the background; all of those fastcgi_param lines allow you flexibility in the variables provided to your application; "listen 3000" says what port the HTTP server will listen on; and fastcgi_pass says how to talk to the fastcgi application. That "location" line says where to attach the FastCGI app.
