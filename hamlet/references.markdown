---
title: References -- Syntax -- Hamlet
---
References give you access to the Haskell world. They do not allow embedding of arbitrary Haskell code, but only simple chains of function calls.

Let me start off by describing a design decision: we want to make code simple for the usual case, and yet offer the ability to do more complicated things when necesary. In particular: usually, people will end up writing Haskell code like this:

    data Person = Person
        { name :: HtmlContent
        , age :: HtmlContent
        }
    data TemplateArgs = TemplateArgs
        { person :: Person
        }

In this case, if we want a reference to the name of the person, we would simply write:

    name.person

This will be converted to the Haskell code:

    name person

where argument is the argument passed to the template, and everything will work out correctly. However, occasionally we'll want to run monadic code. For example:

    lookupOccupation :: Person -> IO HtmlContent

One approach would be to force the user to run all non-pure code before calling the template. While this has its merits, Hamlet wants to allow writing very memory-efficient code, which will sometimes mean interleaving monadic calls. So references allow you to specify that a given function will return a monadic value; in particular:

    *lookupOccupation.person

You can chain monadic and non-monadic calls together as much as you like, Hamlet handles all the juggling. For example:

    address.*home.firstSon.*father.family

would become:

    (father family) >>= home . firstSon >>= return . address

Most people will be using pure functions the majority of the time, in which case references look very much like function application. However, when you need the extra power, it's there.

Please note that when dealing with [loops](loops.html), the asterisk denotes the difference between lists and enumerators. See the [loops](loops.html) page for more details.

### forall and maybe bindings

Within a template, there are two ways to introduce new identifiers: through [$forall](loops.html) and [$maybe](maybe.html). For example:

    name person

would become

    name person

where person is the variable in scope when the Hamlet template is created.  However, let's say you have the following in place:

    $forall people person
        person.name

Then, person would be the values within the people list.
