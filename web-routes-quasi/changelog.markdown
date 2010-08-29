---
title: Changelog -- web-routes-quasi
---
### web-routes-quasi 0.6.0 (August 29, 2010)

* This release reflects changes in web-routes 0.23. In particular, this allows
a type-safe URL to be rendered to both a path and a query-string. This was
needed to support hashed static files in Yesod. Routing itself is still
performed solely on the path piece, ignoring the query-string.

### New in web-routes-quasi 0.5.0

* Major simplifications. Instead of providing a single function to create all
of your TH declarations, it now provides a number of smaller functions. This
should make it more amenable to usage in other frameworks. It also means that
some of the functionality previously found here (such as the Routes type
family) has moved to Yesod.

* Overlap error message doesn't show you all the routes in your application.

* More flexibility in parsing the quasi-quoted definition. There are now two
datatypes: Resource and THResource. The user of this library must define a
function to convert from Resource to THResource. Yesod provides such a
function which does the same job as previous versions of web-routes-quasi.
