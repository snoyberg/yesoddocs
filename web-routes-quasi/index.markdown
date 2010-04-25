---
title: web-routes-quasi
---
Current version: **[0.0.0](http://hackage.haskell.org/package/web-routes-quasi-0.0.0)**. Upcoming version: **none**. Code repo: [http://github.com/snoyberg/web-routes-quasi](http://github.com/snoyberg/web-routes-quasi)

web-routes-quasi is a combination of [quasi-quoting](http://www.haskell.org/haskellwiki/Quasiquotation) and template haskell code to automate generation of URL datatypes and parse, render and dispatch functions for use with the web-routes package.

web-routes is a package which provides support for:

* type-safe URLs

* parsing of request paths to a URL datatype

* render of a URL datatype to a URL string

* dispatch of a request to handler functions

* creating pluggable web components that can be used together

Directly writing all of the necesary functions for web-routes is error-prone and tedious. web-routes-quasi makes this process safe and concise.
