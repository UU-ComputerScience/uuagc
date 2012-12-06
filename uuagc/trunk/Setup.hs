-- Note: to bootstrap uuagc with a commandline uuagc,
-- pass the -DEXTERNAL_UUAGC to GHC
-- when building setup.hs. This can be accomplished using
-- cabal install with --ghc-options="-DEXTERNAL_UUAGC".
--
-- When this option is used, a cabal flag will be set so
-- that the Haskell sources will be regenerated from
-- the attribute grammar sources
--
-- Note: it would be nicer if this behavior could be enabled
-- with a configure flag. However, a compiled Setup.hs is
-- required in order to perform 'configure', so configure
-- flags are regarded too late in the process.
-- Also note that this Setup.hs has conditional package
-- requirements depending on what code is used.

{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}
{-# LANGUAGE CPP #-}
module Main where

#ifdef EXTERNAL_UUAGC
import System.Environment (getArgs)
import Distribution.Simple (defaultMainWithHooksArgs)
import Distribution.Simple.UUAGC (uuagcUserHook)

main :: IO ()
main = args >>= defaultMainWithHooksArgs uuagcUserHook

args :: IO [String]
args = do
  as <- getArgs
  let addFlags | "configure" `elem` as = ("--flags=bootstrap_external" :)
               | otherwise             = id
  return (addFlags as)
#else
import Distribution.Simple (defaultMain, defaultMainWithHooksArgs)

main :: IO ()
main = defaultMain
#endif
