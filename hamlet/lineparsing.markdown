---
title: Line Parsing -- Syntax -- Hamlet
---
There are five special words to start a line: &#36;if, &#36;elseif, &#36;else, &#36;forall and &#36;maybe (described in <a href="$root/hamlet/conditionals.html">conditionals</a>, <a href="$root/hamlet/loops.html">loops</a> and [maybe](maybe.html)). Assuming that one of these words is not present, the following applies:

If a line begins with a backslash, the backslash is dropped, and the rest of the line is treated as content. This allows you to begin a line with a special character, like a period or pound. As a special case, if the line consists entirely of a backslash, it is converted to a newline.

If a line does *not* begin with a percent sign (%), pound (#) or period (.), it is treated as content (see <a href="$root/hamlet/contentparsing.html">content parsing</a>). Otherwise, everything up to the first space to taken as a tag definition. Everything after that first space (if present) is parsed as content and nested within the tag.
