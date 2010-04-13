---
title: Hamlet
---
Hamlet is a templating system loosely based on Haml syntax. Additionally, it is
a monad to support efficient generation of textual data. Since the former is
more usual use case, I will address that first.

Instead of attempting to be a generic templating system, Hamlet attempts to
excel in a specific niche. In particular, the following design goals are
central:

* We only care about HTML output.

* Templates are fully compiled at compile time. What's more, they are embedded in Haskell files via quasi-quoting.

* Constant memory usage is allowed through the use of enumerators. Looping and output both use an enumerator interface. (Actually, output is not *exactly* an enumerator; see the monadic section below for details.)

* We try to avoid the strings problem by forcing all variables to explicitly state whether they require entity encoding.

* All variables are compile-time checked.

If you are generating HTML output from a web application, and you want strong compile-time guarantees of correctness, then this might be for you.
