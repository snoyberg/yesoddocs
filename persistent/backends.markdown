---
title: Backends - Persistent
---
**NOTE: This documentation still reflects persistent version 0.0.0. It will be updated soon.**

A backend is really nothing more than Template Haskell code to create PersistEntity instances based on EntityDefs. The TH code is also responsible for declaring the entity data type.

There are currently three backends: persistent-sqlite, persistent-postgresql and an incomplete persistent-redis. Each of these provide appropriate TH functions.
