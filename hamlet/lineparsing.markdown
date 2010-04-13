---
title: Line Parsing -- Syntax -- Hamlet
---
There are four special words to start a line: &#36;if, &#36;elseif, &#36;else and &#36;forall (described in <a href="$root/hamlet/conditionals.html">conditionals</a> and <a href="$root/hamlet/loops.html">loops</a>). Assuming that one of these words is not present, the following applies:

If a line does *not* begin with a percent sign (%), pound (#) or period (.), it is treated as content (see <a href="$root/hamlet/contentparsing.html">content parsing</a>). Otherwise, everything up to the first space to taken as a tag definition. Everything after that first space (if present) is parsed as content and nested within the tag.
