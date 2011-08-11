module Main where

import Distribution.Simple
import Distribution.Simple.UUAGC

-- uses the bootstrapped version of uuagc to build itself
compiler = "uuagc-bootstrap"
main = defaultMainWithHooks (uuagcUserHook' compiler)
