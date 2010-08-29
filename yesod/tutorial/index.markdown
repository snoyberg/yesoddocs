---
title: Tutorials -- Yesod
---
The documentation on this site refers to the latest version of Yesod, currently 0.5.0. In order to get started:

* Firstly, you'll need [the Haskell Platform](http://hackage.haskell.org/platform/). You should get the most recent version available; due to usage of type families, Yesod requires GHC version 6.12 or higher.

* Get a current list of packages from hackage: cabal update

* Install yesod via cabal: cabal install yesod

* Optional: if you'd like to download all of the examples on this website, type: cabal install yesod-examples

Yesod 0.5.0 comes with an executable (aptly named yesod) which can get a project bootstrapped quickly. After running the above commands, you can just type "yesod"; you'll be asked some basic questions, and your project will be initialized.
