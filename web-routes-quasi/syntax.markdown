---
title: Syntax -- web-routes-quasi
---
Each line represents a single resource pattern. A resource pattern is a list of path pieces, a constructor name, and a handler. Let's look at a quick example, using everyone's favorite demo- a blog:

    /                                     Home      GET
    /entries/$entry                       Entry     GET
    /entries/$entry/comments/#commentid   Comment   PUT DELETE

The first line declares a resource for the root of the web application. The second column names the constructor as Home, and that it handles the GET method.

The next line is more interesting: this is the resource for a blog post. We parse the first column into pieces, slitting at each forward slash. There are four possibilities for each piece:

* If it starts with a dollar sign (&#36;), the piece matches any string. This is called a **string piece**.

* If it starts with a hash (#), the piece matches any integer. This includes negative numbers. This is called an **integer piece**.

* If it starts with an asterick (*), the piece matches all of the remaining pieces. This is only permitted as the last piece in a resource pattern. This is called a **slurp piece**.

* Otherwise, it must be a literal match for the value of the piece. This is called a **static piece**.

For the first three types of pieces, the remaining characters in the piece are completely ignored. So in the second line, "entry" is completely ignored. In the third line, you'll see that we have two static pieces, a string piece and an integer piece.

In all of the lines above, the second column gives the name of the constructor. Let's say that we decided to call the datatype for this site BlogRoutes [see the usage section]($root/web-routes-quasi/usage.html); this would result in a datatype:

    data BlogRoutes = Home
                    | Entry String
                    | Comment String Integer

### Declaring handlers

Everything after the second column declares how to handle the given resource. Assume the value in the second column (the constructor name) is MyRoute. There are three possibilities for the rest of the line:

* If there is nothing after the second column, then a single function named handleMyRoute will be called for al requests to that URL pattern.

* If there are exactly two words after the second column, and the second of those begins with a lowercase letter, we are dealing with a subsite. This allows you to embed functionality written elsewhere within a site. The [synopsis](synopsis.html) shows an example of using a separate module for handling static content. In this case, the first value is the datatype for the subsite routes, and the second is a function that returns a Site value.

* Otherwise, each word is taken as an HTTP request method, and a separate function is called for each method. If the methods specified are GET and DELETE, the functions would be getMyRoute and deleteMyRoute.

This page is meant to only explain the syntax of the quasi-quoted data, not to explain how to write functions that are called by it. That information is [available in the usage section](usage.html).
