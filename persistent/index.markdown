---
title: Persistent
---
Current version: **[0.2.0](http://hackage.haskell.org/package/persistent-0.2.0)**. Code repo: [http://github.com/snoyberg/persistent](http://github.com/snoyberg/persistent)

Persistent is a persistence layer with a huge emphasis on type safety. It uses template haskell to automate the boilerplate production of data types and constructors that allow you to do regular database activities with full compile-time guarantees of type correctness.

It uses a non-relational approach for its API. This is not to say SQL is unsupported; on the contrary, SQL is a first-class target of the library. Instead, it means you are free to break out of the SQL mold and use non-relational databases as a backend. A proof-of-concept Redis backend is in the making.

While persistent is designed to integrate tightly with Yesod, it is quite usable in other contexts.

To use Persistent, you must also use a backend, which provides database-specific code. The currently available backends are:

* [persistent-sqlite 0.2.0](http://hackage.haskell.org/package/persistent-sqlite-0.2.0)

* [persistent-postgresql 0.2.0](http://hackage.haskell.org/package/persistent-postgresql-0.2.0)

There is currently work on both MongoDB and Redis backends being done.
