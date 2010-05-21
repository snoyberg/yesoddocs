---
title: Syntax -- web-routes-quasi
---
Each line represents a single resource pattern. A resource pattern is a list of path pieces, a constructor name, and a handler. Let's look at a quick example, using everyone's favorite demo- a blog:

    /                                         Home      GET
    /entries/#EntryName                       Entry     GET
    /entries/#EntryName/comments/#CommentId   Comment   PUT DELETE

The first line declares a resource for the root of the web application. The second column names the constructor as Home, and that it handles the GET method.

The next line is more interesting: this is the resource for a blog post. We parse the first column into pieces, slitting at each forward slash. There are three possibilities for each piece:

* If it starts with a hash (#), it is a **single piece**. The text after the hash denotes the data type, which must be an instance of SinglePiece. This will parse exactly one path segment.

* If it starts with an asterisk (*), it is a **multi piece**. The text after the asterisk denotes the data type, which must be an instance of MultiPiece. The piece matches all of the remaining pieces. This is only permitted as the last piece in a resource pattern.

* Otherwise, it must be a literal match for the value of the piece. This is called a **static piece**.

The definitions of SinglePiece and MultiPiece are fairly straight-forward:

    class SinglePiece a where
        fromSinglePiece :: String -> Either String a
        toSinglePiece :: a -> String
    class MultiPiece a where
        fromMultiPiece :: [String] -> Either String a
        toMultiPiece :: a -> [String]

web-routes-quasi defines some basic instances, in particular String, Integer and Int instances for SinglePiece and [String] for MultiPiece.

In all of the lines above, the second column gives the name of the constructor. Let's say that we decided to call the datatype for this site BlogRoutes ([see the usage section]($root/web-routes-quasi/usage.html)); this would result in a datatype:

    data BlogRoutes = Home
                    | Entry EntryName
                    | Comment EntryName CommentId

### Declaring handlers

Everything after the second column declares how to handle the given resource. Assume the value in the second column (the constructor name) is MyRoute. There are three possibilities for the rest of the line:

* If there is nothing after the second column, then a single function named handleMyRoute will be called for all requests to that URL pattern.

* If there are exactly three words after the second column, and the second and third of those begin with lowercase letters, we are dealing with a subsite. This allows you to embed functionality written elsewhere within a site. The [synopsis](synopsis.html) shows an example of using a separate module for handling static content. In this case, the first value is the datatype for the subsite argument, the second is a function that returns a QuasiSite value, and the third converts the main sites arg datatype to the subsite's arg datatype.

* Otherwise, each word is taken as an HTTP request method, and a separate function is called for each method. If the methods specified are GET and DELETE, the functions would be getMyRoute and deleteMyRoute. Alternatively, if there is a colon in the word (eg, GET:myGetFunction), the text before the colon is the request method and the text after the colon is the handler function name.

This page is meant to only explain the syntax of the quasi-quoted data, not to explain how to write functions that are called by it. That information is [available in the usage section](usage.html).
