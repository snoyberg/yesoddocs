---
title: References -- Syntax -- Hamlet
---
References are how you refer to template arguments. Under the surface, everything is handled via functions; however, to maintain a more standard look-and-feel, we use a more object-oriented syntax. It's important to remember nonetheless that everything gets converted to function calls at the end of the day.

Let me start off by describing a design decision: we want to make code simple for the usual case, and yet offer the ability to do more complicated things when necesary. In particular: usually, people will end up writing Haskell code like this:

    data Person = Person
        { name :: HtmlContent
        , age :: HtmlContent
        }
    data TemplateArgs = TemplateArgs
        { person :: Person
        }

In this case, if we want a reference to the name of the person, we would simply write:

    person.name

This will be converted to the Haskell code:

    name $ person argument

where argument is the argument passed to the template, and everything will work out correctly. However, occasionally we'll want to run monadic code. For example:

    lookupOccupation :: Person -> IO HtmlContent

One approach would be to force the user to run all non-pure code before calling the template. While this has its merits, Hamlet wants to allow writing very memory-efficient code, which will sometimes mean interleaving monadic calls. So references allow you to specify that a given function will return a monadic value; in particular:

    person.*lookupOccupation

You can chain monadic and non-monadic calls together as much as you like, Hamlet handles all the juggling. For example:

    family.*father.firstSon.*home.address

would become:

    (father $ family argument) >>= home . firstSon >>= return . address

Most people will be using pure functions the majority of the time, in which case references look very much like object-oriented code. However, when you need the extra power, it's there.

Please note that when dealing with [loops](loops.html), the asterisk denotes the difference between lists and enumerators. See the [loops](loops.html) page for more details.

### Forall bindings

In general, a reference is interpreted as a function to be run against the template argument. The one exception is when there is a [forall](loops.html) or [maybe](maybe.html) binding in place. For example:

    person.name

would become

    name $ person argument

However, let's say you have the following in place:

    $forall people person
        person.name

Then, person would be treated as a value instead of a function and the code would be:

    name person
