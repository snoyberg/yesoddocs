---
title: Indentation -- Syntax -- Hamlet
---
Hamlet strays from Haml when it comes to its interpretation of indentation. The rules are very lax, and can be summed up in the following three points:

* A tab is considered four spaces.

* All tabs and space characters from the beginning of a line to the first non-tab, non-space character count as the indentation. If you have 2 spaces and 3 tabs, this counts as an indentation of 14.

* A line is nested within the first line above it with a lower indentation level.

As a simple example, take:

    1 #foo
    2   .bar
    3             something
    4   .baz something else

Line 3 nests within line 2, while lines 2 and 4 nest inside line 1. Notice that line 3 is allowed to have a much larger indentation than lines 2 and 4. The above results in the following HTML:

    <div id="foo"><div class="bar">something</div><div class="baz">something else</div></div>
