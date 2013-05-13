#!/bin/zsh

setopt shwordsplit

function make() {
    cmd="ghc --make -package ghc-7.4.1 -threaded -O -isrc \
        -o $1 -main-is $2 src/$2.hs"
    echo $cmd
    if ! $cmd; then
        exit 1
    fi
}

make ghc-distributor Distributor
make ghc-fe FE
make ghc-compiler Compiler
