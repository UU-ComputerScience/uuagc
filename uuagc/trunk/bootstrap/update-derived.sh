#!/bin/sh

# removes some lines in the ../uuagc.cabal file
# renames the x-bootstrap-XXX lines to XXX

echo "updating src-derived with ../dist/build/uuagc/uuagc-tmp/*.hs"

cp -f ../dist/build/*.hs src-derived/

echo "src-derived updated."
echo "this is the SVN status on src-derived:"

svn status src-derived
