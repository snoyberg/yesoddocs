#!/bin/sh
for f in snippets/*.hs
do
    mkdir -p snippets/tmp/$f
    ghc --make $f -outputdir snippets/tmp/$f -o snippets/tmp/$f-exe
done
