---
title: Overview - Persistent
---
Persistent essentially consists of three different components:

* A high-level interface for [defining entities](defining-entities.html).

* [Type classes](type-classes.html) for actually interacting with a database.

* [Template Haskell code](backends.html) to create data types and instances based on the high-level interface. Some of this code is helper code shared by all backends, some of it is backend-specific.
