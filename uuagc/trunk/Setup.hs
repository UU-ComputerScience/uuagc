-- Note: to bootstrap uuagc with a commandline uuagc,
-- pass the -DEXTERNAL_UUAGC to GHC
-- when building setup.hs. This can be accomplished using
-- cabal install with --ghc-options="-DEXTERNAL_UUAGC".
--
-- When this option is used, a cabal flag will be set so
-- that the uuagc build does not depend on the
-- uuagc-bootstrap module.
--
-- Note: it would be nicer if this behavior could be enabled
-- with a configure flag. However, a compiled Setup.hs is
-- required in order to perform 'configure', so configure
-- flags are regarded too late in the process.
-- Also note that this Setup.hs has conditional package
-- requirements depending on what code is used.

{-# LANGUAGE CPP #-}
module Main where

import Distribution.Simple
import Distribution.Simple.UUAGC
import System.Environment

#ifdef EXTERNAL_UUAGC
compiler = uuagcFromString "uuagc"
args = do
  as <- getArgs
  let addFlags | "configure" `elem` as = ("--flags=bootstrap_external" :)
               | otherwise             = id
  return (addFlags as)
#else
-- uses the bootstrapped version of uuagc to build itself
import UU.UUAGC.Bootstrap
compiler = uuagcBootstrap
args = getArgs
#endif

main :: IO ()
main = args >>= defaultMainWithHooksArgs (uuagcLibUserHook compiler)
