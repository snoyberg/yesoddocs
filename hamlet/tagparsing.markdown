---
title: Tag Parsing -- Syntax -- Hamlet
---
We break up a string at each delimiter. A delimiter is one of: %#.!

A piece beginning with a percent sign denotes the tag name; beginning with a pound denotes the id attribute; beginning with a period is a class attribute.

A piece beginning with an exclamation mark denotes an arbitrary attribute, parsed as follows: everything up to the first equal sign is the attribute key. If there is no equal sign, it is printed as an empty attribute. For example:

    %input!type=checkbox!checked

becomes

    <input type="checkbox" checked>

If there is an equal sign, everything following it is parsed as content (as per the next section).
