---
title: Content Parsing -- Syntax -- Hamlet
---
There are three special characters in content: @$^. These must appear in pairs in content. If the pair appears right next to each other, such as @@, then a single character is output. In other words,

    michael@@snoyman.com

becomes

    michael@snoyman.com

Otherwise, any text between the pair of delimiters is treated as a "reference". References are a series of period-separated identifiers, such as:

    family.father.name

Conceptually, this means "get the name value from the father value from the family value." In other words, you can pretend that this is object-oriented. However, it's important to understand how this works under the surface. Since we are dealing with Haskell, we treat these identifiers as functions. For the most part, the above reference will be converted to the following Haskell code:

    family argument >>= father >>= name

where argument is the argument you pass to the template. Notice that everything here is monadic; in particular, each of these functions must work within the Hamlet monad. So a valid set of type signatures for this fragment would be:

    argument :: Household
    family :: Household -> Hamlet url IO Family
    father :: Family -> Hamlet url IO Person
    name :: Person -> Hamlet url IO HtmlContent

There is one exception to the above rule: if a forall binds a value to the first identifier in a chain, that is used instead. foralls will be addressed properly below, but for example's sake:

    %ul
        $forall people person
            %li $person.name$

Here, you do not need a person function to be defined.

Now that we've addressed references, what's the difference between the three delimiters (&#36;@^) mentioned above? The dollar sign ($$) outputs HtmlContent; the at sign (@) outputs URLs; and the caret (^) embeds other templates. In other words, they each (respectively) require a type signature of:

    Hamlet url m HtmlContent
    Hamlet url m url
    Hamlet url m ()
