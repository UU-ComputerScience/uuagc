rm -rf dist-newstyle
set -e
cabal v2-build -fbootstrap_external -fwith-loag
cp dist-newstyle/build/x86_64-linux/ghc-*/uuagc-*/build/*.hs src-generated/
cp dist-newstyle/build/x86_64-linux/ghc-*/uuagc-*/build/LOAG/*.hs src-generated/LOAG/
# Patch the line pragma's a bit
find src-generated -type f -name "*.hs" -exec sed -i \
   's/{-# LINE \([0-9]\+\) ".*\/build\/\(.*\.hs\)" *#-}/{-# LINE \1 "src-generated\/\2" #-}/' {} \;
