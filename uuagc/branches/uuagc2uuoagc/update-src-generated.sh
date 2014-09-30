rm -rf dist
cabal configure --ghc-options="-DEXTERNAL_UUAGC" -fwith-loag
cabal build --ghc-options="-DEXTERNAL_UUAGC"
cp dist/build/*.hs src-generated/
cp dist/build/LOAG/*.hs src-generated/LOAG/
