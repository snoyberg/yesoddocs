---
title: Deploying -- Yesod
---
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
