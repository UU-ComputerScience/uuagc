#!/bin/sh

# removes some lines in the ../uuagc.cabal file
# renames the x-bootstrap-XXX lines to XXX

sed -e '/^name\:/d' \
    -e '/^version\:/d' \
    -e '/^build\-type\:/d' \
    -e '/ exposed\-modules\:/d' \
    -e '/ hs\-source\-dirs\:/d' \
    -e '/build\-depends\: uuagc-bootstrap/d' \
    -e '/uuagc\-cabal/d' \
    -e '/^extra-source-files: uuagc_options/d' \
    -e 's/executable uuagc/executable uuagc\-bootstrap/' \
    -e 's/x\-bootstrap\-//' ../uuagc.cabal > uuagc-bootstrap.cabal

echo "uuagc-bootstrap.cabal generated."
