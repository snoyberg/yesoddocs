Routing is the process of associating a requested URL to a handler function. In the "good ol' days" of CGI scripts, this was handled by file and directory names. There are *many* downsides to this approach; some simple ones are that it's difficult to pass in parameters via the URL, and therefore query strings tend to be abused, and that you have to write many smaller scripts instead of a single application. These same arguments apply to plain PHP and ASP pages.

Many modern frameworks use the approach of regular expressions. In these, you define the routing rules in a central place, and every request to the application gets routed through there. This is a huge step forward, but I'm sure many of you have heard this quote before:

> Some people, when confronted with a problem, think “I know, I'll use regular expressions.”  Now they have two problems.

In most cases of web development, all we care about are breaking a URL into pieces at each forward slash and routing based on the resulting list of strings. And this is exactly the approach Yesod takes. This chapter will have a little bit of overlap with some of the material in the [basics chapter](/book/basics/), but will go into more detail.

## Routes syntax

We've already seen routes declarations in previous chapters; let's have a little bit of a review:

    /              RootR     GET
    /name/#String  NameR     GET POST
    /age/#Int      AgeR      GET PUT

Each line in a routes declaration declares an individual **resource pattern**. Each line can be broken down into three parts: the pattern, the name and the routing information. The name is the simplest: it becomes the name of a constructor in the route data type (more on this later). By convention, route names end with a capital R, but you are free to ignore this convention.

The last part of the line is the routing information. In the examples above, it's a list of HTTP request methods that the resource pattern will respond to. This is what you'll use most commonly in Yesod. There are two other options: if you leave this blank, then you can respond to *all* request methods, and later we'll discuss how to use subsites.

For the moment, it's the pattern that we'd like to analyze. A pattern is broken down into pieces at each forward slash. RootR has no pieces, while NameR and AgeR each have two pieces. There are three types of pieces:

* A static piece is simply a word that matches literally.

* A single piece matches against one piece. It begins with a hash (#) and is followed by a data type. The data type must be an instance of the SinglePiece typeclass.

* A multi piece matches against the remainder of the pieces. It begins with an asterisk (*) and, like a single piece, is followed by a data type. The data type must be an instance of the MultiPiece typeclass.

The SinglePiece and MultiPiece typeclasses are simply provided for converting to and from String and [String], respectively. Their definitions are simple:

    class SinglePiece s where
        fromSinglePiece :: String -> Either String s
        toSinglePiece :: s -> String
    class MultiPiece s where
        fromMultiPiece :: [String] -> Either String s
        toMultiPiece :: s -> [String]

Converting *to* a String (or [String]) must always succeed, but the reverse is not true. This allows us to create instances for types like numbers. In fact, our example aboves exploits this with the AgeR resource pattern.

Multipiece might be a bit more elusive. A concrete example would be the pages of a wiki: we're allowed to have hierarchies of "nested folders" in the names of the pages, so we might define our routes as follows:

~routing-wiki

### Subsites

Subsites are an important concept in Yesod that allow us to create composible sets of pages that can be used in multiple sites. Some prime examples are included in Yesod itself: static files and user authentication. We'll cover more details later, but for now let's just see the syntax:

    /static StaticR Static getStatic

As before, the first part of the line is the pattern. With subsites, you can *only* use static pieces in the pattern; any dynamic parts of the URL must be handled by the subsite itself. In our case, we are setting up /static as the root of our new subsite. All URLs for that subsite will begin with /static. Also as before, the second word is the name of the resource pattern.

The last two words are important. The first is the datatype for the **subsite argument**. Just like a Yesod application has a site argument datatype, each subsite has its own datatype. This can be used to pass configuration settings. In the case of the static subsite, Static contains information on where to find the static files to be served.

The final word on the line is a function to get a subsite value from the master site value. All of this might be clearer with a little example:

~routing-static

## parseRoutes

Obviously, the syntax explained above for describing routes is not Haskell, and therefore can't be directly inserted into your source code. This is another place where Yesod uses **quasi-quotation** to allow the embedding of an arbitrary syntax. You've seen quasi-quotation [previously](/book/templates/) with Hamlet.

parseRoutes is provided by the web-routes-quasi package in the Web.Routes.Quasi.Parse module. It does two important things: it converts your routes definition into a value of type [Resource], and then checks if there are any overlapping routes. For example, the following routes would overlap, since /foo/bar would match them both:

    /foo/#String
    /#String/bar

If you would like to ignore the overlaps check and simply go with the first match, you can use the parseRoutesNoCheck function.

It's also possible to completely bypass the quasi-quoter and directly generate the list of resources. For example:

~routing-noquasi

## mkYesod

The last piece of the puzzle is that mkYesod function. It is actually a piece of Template Haskell that takes the name of the site argument and a list of resources and generates the following:

### Route datatype

Also known as the type-safe URL. If your site argument is called MyApp, the route datatype will be MyAppRoute. There will be a constructor for each resource pattern, with the name you specified. In the case of a subsite, that constructor will take a single argument: the route datatype for the subsite. Otherwise, there will be one argument to the constructor for each single and multi piece. For example:

    / HomeR GET
    /name/#String NameR GET
    /age/#Int AgeR GET
    /static StaticR Static getStatic
    /location/#City/#State LocationR GET
    /wiki/*WikiPage WikiR GET POST

will create a datatype:

    data MyAppRoute =
        HomeR
      | NameR String
      | AgeR Int
      | StaticR Static
      | LocationR City State
      | WikiR WikiPage

mkYesod also automatically creates a **type family instance** associating the site argument with the route datatype. In other words, it creates a synonym between:

    Route MyApp

and

    MyAppRoute

This can be useful for writing generic functions that work for multiple sites.

## YesodSite instance

There are three other pieces of information that we need to make this whole thing work:

* How to convert a URL to a value of the route datatype. This is *parsing* a route.

* How to convert a value of the route datatype to a URL. This is *rendering* a route.

* How to respond to requests on this URL. This is *dispatching* a route.

These three functionalities are combined into a Site datatype in the web-routes package. Yesod has a YesodSite typeclass to get the Site for a site argument. mkYesod also creates this instance.

## Splitting things up

It's great having all of this functionality in a single function, but this can also present a difficulty. Most handler functions will depend on the site argument datatype, and the YesodSite instance will depend on the handler functions. If we use the mkYesod function, we'll need to include all of the handlers and the call to mkYesod in the same file, which can be a bit unwieldy.

Yesod provides two additional functions: mkYesodData creates just the datatype and mkYesodDispatch creates the YesodSite instance. mkYesodData also creates a variable containing the list of routes so that you only need to declare your routes once.

The site template created by the scaffolding tool automatically sets up the calls to mkYesodData and mkYesodDispatch appropriately.

## Conclusion

A defining feature of Yesod is type-safe URLs. A common deterrent to creating such a robust system is the boilerplate and clumsiness of creating a datatype, parse, render and dispatch functions that are all consistent. By using code generation, Yesod makes this powerful feature easily accessible.

We have not yet fully covered the concept of subsites referenced in this chapter; the full concept will take a chapter of its own. For the moment, hopefully you'll be able to get started using some of the subsites built into Yesod.
