module Distribution.Simple.UUAGC.AbsSyn where

import Options
import System.FilePath(normalise)

data AGFileOption = AGFileOption {filename :: String,
                                  fileClasses :: [String],
                                  opts :: Options}

data AGOptionsClass = AGOptionsClass {className :: String, opts' :: Options}

type AGFileOptions = [AGFileOption]

lookupFileOptions :: FilePath -> AGFileOptions -> Options
lookupFileOptions s = foldl f noOptions
    where f e (AGFileOption s' classes opt)
              | s == (normalise s')  = opt
              | otherwise            = e
