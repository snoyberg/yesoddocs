---
title: References -- Syntax -- Hamlet
---
References give you access to the Haskell world. They do not allow embedding of arbitrary Haskell code, but only simple chains of function calls.

Let's say you have the following Haskell code:

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

where argument is the argument passed to the template, and everything will work out correctly.

**NOTE**: Previous versions of Hamlet allowed embedding of monadic code in templates. This feature was not used very often and introduced a significant performance hit, and was removed in version 0.3.0.

### forall and maybe bindings

Within a template, there are two ways to introduce new identifiers: through [$forall](loops.html) and [$maybe](maybe.html). For example:

    name.person

would become

    name person

where person is the variable in scope when the Hamlet template is created.  However, let's say you have the following in place:

    $forall people person
        name.person

Then, person would be the values within the people list.
