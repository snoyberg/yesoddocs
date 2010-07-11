---
title: Changelog -- Yesod
---
### New in Yesod 0.4.0 (not yet released)

A big thanks on this release to Simon Michael, who pointed out a number of
places where the docs were unclear, the API was unintuitive, or the names were
inconsistent.

* Widgets. These allow you to create composable pieces of a webpage that
keep track of their own Javascript and CSS. It includes a function for
obtaining unique identifiers to avoid name collissions, and does automatic
dependency combining; in other words, if you have two widgets that depend on
jQuery, the combined widget will only include it once.

* Combined the Yesod.Form and Yesod.Formable module into a single, consistent,
widget-based API. It includes basic input functions as well as fancier
Javascript-driven functions; for example, there is a plain day entry field,
and a day entry field which automatically loads the jQuery UI date picker.

* Cleaned up a bunch of API function names for consistency. For example,
Yesod.Request now has a logical lookupGetName, lookupPostName, etc naming
scheme.

* Changed the type of basicHandler to require less typing, and added
basicHandler' which allows you to modify the line output to STDOUT (or skip it
altogether).

* Switched the Handler monad from ContT to MEitherT (provided by the neither
package). ContT does not have a valid MonadCatchIO instance, which is used for
the sqlite persitent backend.

* Facebook support in the Auth helper.

* Ensure that HTTP request methods are given in ALL CAPS.

* Cleaned up signatures of many methods in the Yesod typeclass. In particular,
due to changes in web-routes-quasi, many of those functions can now live in
the Handler monad, making it easier to use standard functions on them.

* The static file helper now has extensible file-extension-to-mimetype
mappings.

* Added the sendResponse function for handler short-circuiting.

* Renamed Routes to Route.
