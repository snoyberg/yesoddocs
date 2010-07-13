---
title: Changelog -- Hamlet
---
### New in Hamlet 0.4.0 (not yet released)

* Internal template parsing is now done via Parsec. This opened the doors for
the other changes mentioned below, but also hopefully gives more meaningful
error messages. There's absolutely no runtime performance hit for this change,
since all parsing is done at compile time, and if there *is* any compile-time
hit, it's too negligible to be noticed.

* Attribute values can now be quoted. This allows you to embed spaces, periods
and pounds in an attribute value. For example:
[$hamlet|%input!type=submit!value="Add new value"|].

* Space-delimited references in addition to period-delimited ones. This only
applies to references in content, not in statements. For example, you could
write [$hamlet|$foo bar baz$|].

* Dollar-sign interpolation is now polymorphic, based on the ToHtml typeclass.
You can now do away with $string.var$ and simply type $var$. Currently, the
ToHtml typeclass is not exposed, and it only provides instances for String and
Html, though this is open for discussion.

* Added hamletFile and xhamletFile which loads a Hamlet template from an
external file. The file is parsed at compile time, just like a quasi-quoted
template, and must be UTF-8 encoded. Additionally, be warned that the compiler
won't automatically know to recompile a module if the template file gets
changed.
