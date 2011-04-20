#!/bin/sh
for f in snippets/*.hs
do
    if [ $f != 'snippets/example-hamlet1.hs' ]
    then
        mkdir -p snippets/tmp/$f
        ghc --make $f -outputdir snippets/tmp/$f -o snippets/tmp/$f-exe || exit
    fi
done
