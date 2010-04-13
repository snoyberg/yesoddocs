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

* Templates are fully compiled at compile time. What's more, they are embedded in Haskell files via [quasi-quoting](http://www.haskell.org/haskellwiki/Quasiquotation).

* Constant memory usage is allowed through the use of enumerators. Looping and output both use an enumerator interface. (Actually, output is not *exactly* an enumerator; see <a href="$root/hamlet/monad.html">the monadic interface section</a> for details.) There are many flavors of enumerators out there; this is very close to the definition used in WAI, and I recommend you read [the documentation there](http://hackage.haskell.org/packages/archive/wai/0.0.0/doc/html/Network-Wai.html#8) for details.

* We try to avoid [the strings problem](http://blog.moertel.com/articles/2006/10/18/a-type-based-solution-to-the-strings-problem) (aka XSS attacks) by forcing all variables to explicitly state whether they require entity encoding.

* All variables are compile-time checked.

If you are generating HTML output from a web application, and you want strong compile-time guarantees of correctness, then this might be for you.
