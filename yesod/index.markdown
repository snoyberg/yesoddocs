---
title: Yesod
---
Current version: **[0.3.1](http://hackage.haskell.org/package/yesod-0.3.1)**. Upcoming version: **0.4.0**. Code repo: [http://github.com/snoyberg/yesod](http://github.com/snoyberg/yesod)

Yesod is a web framework for the Haskell programming language. It has the following design goals:

* Build [RESTful](http://en.wikipedia.org/wiki/Representational_State_Transfer) web sites: single URL for each resource, use request methods properly, and multiple representations of data.

* Have as many compile-time guarantees of correctness as possible, including type-safe URLs and compile-time parsed templates.

* Allow concise coding through a combination of well-designed APIs and [quasi-quoted](http://www.haskell.org/haskellwiki/Quasiquotation) DSLs.

* Creation of reusable web components (known as subsites).

* Space-efficiency without complications by using enumerators under the scenes and providing higher-level abstractions over them.

* Not tie you down to any server system: Yesod applications run happily on anything from dedicated hosting to [low-end shared hosting](http://www.nearlyfreespeech.net/).

The framework loosely follows MVC principles; however, there is no desire to strictly adhere to any set of theoretical principles. The goal is play to the strengths of Haskell to allow users to create secure, fast and featureful websites with minimal code overhead.

The documentation on this site refers to version 0.3.0 of Yesod. In order to get started, install [the Haskell Platform](http://hackage.haskell.org/platform/) (with GHC 6.12), then run:

    cabal update
    cabal install yesod
