---
title: Conditionals -- Syntax -- Hamlet
---
As mentioned above, $if, $elseif and $else are treated specially. As a simple example:

    $if employed.person
        Employed
    $elseif retired.person
        Retired
    $else
        Unemployed

In this case, employed.person and retired.person are parsed as references (see Content Parsing above for explanation). In this case, they must return values of type Bool.
