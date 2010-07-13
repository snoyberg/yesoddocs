---
title: Changelog -- persistent
---
### New in persistent 0.1.0

* Split up the PersistEntity typeclass into PersistBackend and PersistEntity.
This makes the TH generation code universal to all backends, and means backend
code doesn't require any TH wizardry.

* Added connection pool support.

* Replaced the Hamlet dependency with a blaze-html dependency.

* Attribute values available on entity as well as columns.

* You can have quoted attribute values, which allow multi-word attributes.

* Arbitrary SQL table and column name mapping using the "sql=name" attribute.
