---
title: Line Parsing -- Syntax -- Hamlet
---
There are four special words to start a line: $if, $elseif, $else and $forall. See below for an explanation of these. Assuming that one of these words is not present, the following applies:

If a line does *not* begin with a percent sign (%), pound (#) or period (.), it is treated as content. See below for the parsing of content. Otherwise, everything up to the first space to taken as a tag definition. Everything after that first space (if present) is parsed as content and nested within the tag.
