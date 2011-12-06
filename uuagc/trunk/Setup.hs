module Main where

import Distribution.Simple
import Distribution.Simple.UUAGC
import UU.UUAGC.Bootstrap

-- uses the bootstrapped version of uuagc to build itself
compiler = uuagcBootstrap
main = defaultMainWithHooks (uuagcLibUserHook compiler)
