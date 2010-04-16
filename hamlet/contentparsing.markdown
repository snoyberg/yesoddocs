---
title: Content Parsing -- Syntax -- Hamlet
---
There are three special characters in content: @$^. These must appear in pairs in content. If the pair appears right next to each other, such as @@, then a single character is output. In other words,

    michael@@snoyman.com

becomes

    michael@snoyman.com

Otherwise, any text between the pair of delimiters is treated as a "reference". Please read [references](references.html) for a full description.

What's the difference between the three delimiters (&#36;@^) mentioned above? The dollar sign ($$) outputs HtmlContent; the at sign (@) outputs URLs; and the caret (^) embeds other templates. In other words, they each (respectively) require a return type of:

    HtmlContent
    url
    Hamlet url m ()
