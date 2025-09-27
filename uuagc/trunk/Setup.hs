{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}
module Main where

import System.Environment (getArgs)
import Distribution.Simple (defaultMainWithHooksArgs, UserHooks (..), simpleUserHooks)
import Distribution.Simple.LocalBuildInfo (flagAssignment)
import Distribution.Simple.UUAGC (uuagcUserHook)
import Distribution.Types.Flag (lookupFlagAssignment, mkFlagName)
import Debug.Trace

main :: IO ()
main = do
  args <- getArgs
  defaultMainWithHooksArgs hooks args
  where
    hooks = uuagcUserHook { buildHook = myBuildHook }
    myBuildHook pd lbi uh bf
      | lookupFlagAssignment (mkFlagName "bootstrap_external") (flagAssignment lbi) == Just True
      = buildHook uuagcUserHook pd lbi uh bf
      | otherwise
      = buildHook simpleUserHooks pd lbi uh bf