* Atom feeds
* JSON (Ajax example)
* Request methods
* Representations

# FIXME old content

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
