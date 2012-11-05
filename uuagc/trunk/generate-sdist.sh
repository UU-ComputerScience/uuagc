# Stolen from happy
rm -rf dist
cabal configure --ghc-options="-DEXTERNAL_UUAGC"
cabal build --ghc-options="-DEXTERNAL_UUAGC"
rm -f dist/uuagc-*.tar.gz
rm -rf dist/uuagc-*/
dist/setup/setup sdist
cd dist
tar xvzf uuagc-*.tar.gz
cd uuagc-*
mkdir src-generated
cd ..
cp build/*.hs uuagc-*/src-generated/
tar cvzf uuagc-*.tar.gz uuagc-*/
