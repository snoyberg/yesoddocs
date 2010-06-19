---
title: Hamlet
---
Current version: **[0.3.0](http://hackage.haskell.org/package/hamlet-0.3.0)**. Upcoming version: **none**. Code repo: [http://github.com/snoyberg/hamlet](http://github.com/snoyberg/hamlet)

Hamlet is a templating system loosely based on Haml syntax. Additionally, it is
a monad to support efficient generation of textual data.

Instead of attempting to be a generic templating system, Hamlet attempts to
excel in a specific niche. In particular, the following design goals are
central:

* We only care about HTML output.

* Templates are fully compiled at compile time. What's more, they are embedded in Haskell files via [quasi-quoting](http://www.haskell.org/haskellwiki/Quasiquotation).

* Hamlet is built on top of the BlazeHtml library, meaning very good performance and interoperability with that library. Since Hamlet can do a good deal of work at compile time, it seems to be faster than using BlazeHtml directly.

* We try to avoid [the strings problem](http://blog.moertel.com/articles/2006/10/18/a-type-based-solution-to-the-strings-problem) (aka XSS attacks) by forcing all variables to explicitly state whether they require entity encoding.

* All variables are compile-time checked.

If you are generating HTML output from a web application, and you want strong compile-time guarantees of correctness, then this might be for you.
