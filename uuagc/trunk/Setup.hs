{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}
module Main where

import Distribution.Simple (defaultMainWithHooks)
import Distribution.Simple.UUAGC (uuagcUserHook)

main :: IO ()
main = defaultMainWithHooks uuagcUserHook
