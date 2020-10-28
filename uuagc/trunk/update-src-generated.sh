rm -rf dist-newstyle
cabal v2-configure --ghc-options="-DEXTERNAL_UUAGC" -fwith-loag
cabal v2-build --ghc-options="-DEXTERNAL_UUAGC"
cp dist-newstyle/build/x86_64-linux/ghc-*/uuagc-*/build/*.hs src-generated/
cp dist-newstyle/build/x86_64-linux/ghc-*/uuagc-*/build/LOAG/*.hs src-generated/LOAG/
# Patch the line pragma's a bit
find src-generated -type f -name "*.hs" -exec sed -i \
   's/{-# LINE \([0-9]\+\) ".*\/build\/\(.*\.hs\)" *#-}/{-# LINE \1 "src-generated\/\2" #-}/' {} \;
