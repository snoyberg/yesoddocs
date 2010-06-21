---
title: Persistent
---
Current version: **none**. Upcoming version: **0.0.0**. Code repo: [http://github.com/snoyberg/persistent](http://github.com/snoyberg/persistent)

Persistent is a persistence layer with a huge emphasis on type safety. It uses template haskell to automate the boilerplate production of data types and constructors that allow you to do regular database activities with full compile-time guarantees of type correctness.

It uses a non-relational approach for its API. This is not to say SQL is unsupported; on the contrary, SQL is a first-class target of the library. Instead, it means you are free to break out of the SQL mold and use non-relational databases as a backend. A proof-of-concept Redis backend is in the making.

While persistent is designed to integrate tightly with Yesod, it is quite usable in other contexts.
