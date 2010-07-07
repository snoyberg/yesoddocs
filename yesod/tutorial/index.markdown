---
title: Tutorials -- Yesod
---
The documentation on this site refers to the latest "bleeding edge" version of Yesod, currently 0.4.0. This version has not been released to Hackage yet, so it takes a bit more work to get started. The API is not yet stable, but if you'd like to get started, just follow these steps:

* Firstly, you'll need [the Haskell Platform](http://hackage.haskell.org/platform/). You should get the most recent version available; due to usage of type families, Yesod requires GHC version 6.12 or higher.

* Download [git](http://git-scm.com/) for getting the development versions of the packages.

* Run the following incantation, which updates your package list from Hackage and installs the four packages necessary from git repositories:

&nbsp;

    cabal update
    git clone http://github.com/snoyberg/hamlet.git
    cd hamlet
    cabal install
    cd ..
    git clone http://github.com/snoyberg/web-routes-quasi.git
    cd web-routes-quasi
    cabal install
    cd ..
    git clone http://github.com/snoyberg/persistent.git
    cd persistent
    cabal install
    cd backends/sqlite
    cabal install
    cd ../../..
    git clone http://github.com/snoyberg/yesod.git
    cd yesod
    cabal install
    cd ..

The source code for this website is in fact a cabal package containing all of the tutorials as executables; if you'd like to grab these as well, run:

    git clone http://github.com/snoyberg/yesoddocs.git
    cd yesoddocs
    cabal install
    cd ..
