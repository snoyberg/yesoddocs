---
title: Conditionals -- Syntax -- Hamlet
---
As mentioned above, $if, $elseif and $else are treated specially. As a simple example:

    $if person.employed
        Employed
    $elseif person.retired
        Retired
    $else
        Unemployed

In this case, person.employed and person.retired are parsed as references (see Content Parsing above for explanation). In this case, they must return values of type Bool.
