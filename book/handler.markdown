---
title: The Handler Monad
---
Most of the code you write in Yesod lives within the Handler monad, so it's important to understand what it's doing. Monads have a reputation for being difficult to understand; if you are intimidated by monads, have no fear of this chapter (or of the Handler monad itself). You needn't have an understanding of monads to use them. In fact, my advice is to not bother trying to understand them at first. After usage, understanding follows.

Monads provide a method for encapsulating certain functionalities. There are a number of "standard" monads out there: reader provides some data which doesn't change, state provides data which can be changed, writer "logs" information, and error allows early termination of a function with a special return value. (Contrary to its name, the error monad doesn't need to be about errors.)

You can also create "monad transformer stacks", which combine a number of different monads together to encapsulate different types of functionalities together. For example, you might have a monad for interacting with a database: you could use a reader to keep the database connection, and writer to log query results, and error to exit early on database failure.

Handler is one such monad transformer stack. It uses a reader to track information on the request from the user, a writer for response headers to be sent back, error for special responses, and a few more pieces as well. As implied above, it's not important to understand **how** Handler works, but rather how to use it.

### There is no Handler

OK, I've been lying to you this whole time. Yesod does not actually export a datatype called Handler. Instead, it exports GHandler, which stands for "Generic Handler." When you use the scaffolding tool, a type synonym is create for you called Handler. If your **site argument** is called MyApp, then Handler is defined as:

    type Handler = GHandler MyApp MyApp

<p class="advanced">The reason MyApp needs to be specified twice is to do with subsites, an advanced topic we're going to ignore for the moment. Subsites actually pervade a lot of the design of Yesod, but you are shielded from this most of the time. We'll discuss subsites in a later chapter.</p>

For the remainder of this chapter, we'll assume that you have the Handler data type defined.

### Handler versus handler function

There are two very related terms: the Handler monad and a handler function. A handler function is a function called to generate a response to a request. A handler function always returns a value in the Handler monad. In fact, this value must be an instance of the HasReps typeclass which we have [discussed earlier](basics.html).

So to refer to our hello world example from before, the type signature for getHomeR:

    getHomeR :: Handler RepPlain

## Short circuiting

I mentioned that Handler includes error monad functionality. This allows it to **short circuit** the handler function, meaning to return a result immediately. There are a number of different short-circuiting responses; notFound is probably the simplest.

    getPersonR name = do
        if name == "michael"
            then return ()
            else notFound -- we only know one guy around here
        defaultLayout $ addBody $(hamletFile "michael")

As you might guess, notFound simply returns a 404 response. Other similar functions are permissionDenied and invalidArgs, which return 403 and 400 status codes, respectively. Their usage is similarly simple:

    postFireTheMissilesR target = do
        hasPrivileges <- canFireMissiles
        if hasPrivileges
            then return ()
            else permissionDenied "We don't let just anyone fire missiles."
        case target of
            "Romulans" -> fireTheMissiles Romulans
            "Klingons" -> fireTheMissiles Klingons
            _ -> invalidArgs ["Unknown target: " ++ target]

## Redirecting

Those previous three functions definitely count as error responses. In fact, the whole 4xx series of status codes are error codes. But I said at the beginning that the error monad works for things besides errors. So as not to disappoint, I introduce redirecting.

    getHomeR = do
        maybeUid <- maybeAuthId -- see if the user is logged in
        case maybeUid of
            Nothing -> defaultLayout $ addBody $(hamletFile "homepage")
            -- send a logged in user to his/her profile page
            Just uid -> redirect RedirectTemporary $ ProfileR uid


