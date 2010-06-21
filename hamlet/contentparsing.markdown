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

### Query strings

Sometimes, when displaying a URL, you will want to attach a query string to it. For this, there is a modification of the at sign (@) available. If you place a question mark (?) after the first at sign, the expected return type is instead:

    (url, [(String, String)])

Hamlet automatically handles percent and HTML encoding the parameters.

### Trailing dollar signs

**New in 0.3.1**. Sometimes you will want to have a trailing space on a line. For example:

    %p
        My name is<!-- trailing space desired -->
        %b Michael
        \.

Hamlet allows you to simply leave a space on the end of the line, no questions asked, and you'll end up with the right HTML, specifically:

    <p>My name is <b>Michael</b>.</p>

However, some text editors will remove trailing spaces. Additionally, a lot of people (myself included) use settings in their editors to flag trailing spaces, and some VCS tools like Git don't like them either. Therefore, Hamlet allows you to leave a trailing dollar sign, which will simply be ignored. So:

    %p
        My name is $
        %b Michael
        \.

The reason for using the dollar sign is twofold:

* It already has connotations of "end-of-line" from its usage in regular expressions.

* The dollar sign is already treated specially by Hamlet. In particular, dollar signs must always be inserted in pairs. This means we can add this extra rule without changing the behavior of Hamlet otherwise.
