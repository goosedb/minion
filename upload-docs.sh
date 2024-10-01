#!/bin/sh
set -e
dir=$(mktemp -d dist-docs.XXXXXX)
trap 'rm -r "$dir"' EXIT
cabal v2-haddock $1 --builddir="$dir" --haddock-for-hackage --enable-doc
cabal upload -d --publish $dir/$1-*-docs.tar.gz --verbose