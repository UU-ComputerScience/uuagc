# Stolen from happy
cabal configure
cabal build
rm -f dist/uuagc-*.tar.gz
rm -rf dist/uuagc-*/
dist/setup/setup sdist
cd dist
tar xvzf uuagc-*.tar.gz
cp build/*.hs uuagc-*/dist/build/
tar cvzf uuagc-*.tar.gz uuagc-*/
