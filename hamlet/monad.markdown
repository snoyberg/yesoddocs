---
title: Monadic Interface -- Syntax -- Hamlet
---
Most users will be happy to just use the quasi-quoter; however, at a certain point, you have to deal with the resulting Hamlet value. This support can be baked into a framework (like [Yesod](http://www.yesodweb.com/)), but someone has to eventually write *that* support. If you're that somebody, this section is for you.

However, even if you don't want to use the Hamlet syntax and quasi-quoter, the monadic interface itself offers you benefits:

* The enumerator interface provides excellent performance.

* By supporting this interface, you can interoperate easily with Hamlet templates.

* You have built in support for url to String conversions to get great type-safe URLs and work well with web-routes.

* The HtmlContent type helps you solve the string problem in the escaping of HTML entities.

So let's look at the actual datatype of Hamlet:

    type Iteratee val seed m = seed -> val -> m (Either seed seed)

    newtype Hamlet url m a = Hamlet
        { runHamlet :: forall seed.
           (url -> String)
        -> seed
        -> Iteratee Text seed m
        -> m (Either seed (a, seed))
        }

This looks confusing, but it's not too bad. Hamlet is a newtype so that it can define its own Monad instance, and to be existential over the seed value without using deprecated extensions.

There are three type variables: url, m and a. a is simply the contained value, like in any monad. Since Hamlet is a monad transformer, m is the inner monad. This will usually be IO.

<p id="type-safe-urls">url has to do with type-safe URLs. We want to take advantage of the type system of Haskell to ensure we generate only valid URLs, so we create a datatype to represent all possible URLs for an application. That way, any value of type url is *guaranteed* to correlate to a valid URL.</p>

Which brings us nicely to the first argument: we can't print out arbitrary data types to HTML documents; we need a String. The first argument (url -> String) provides this conversion.

The next two arguments form the enumerator interface for outputting the HTML document. If you're not familiar with enumerators, I recommend looking at the WAI documentation, which discusses this form of enumerators fairly thoroughly.

The return type is also interesting. Like most enumerators, Left indicates early termination and Right indicates that it ran to completion. However, the Right also contains a value of type a. The reason for this is that- by excluding the a value for early termination- we can skip computing the remaining values once output has been terminated.

### Supporting Functions

The Haddock documentation on the supporting functions in Text.Hamlet.Monad is hopefully comprehensive enough to be understandable. Keep in mind that they were geared towards the support of Hamlet syntax, thus the slightly odd type signature for condH.
