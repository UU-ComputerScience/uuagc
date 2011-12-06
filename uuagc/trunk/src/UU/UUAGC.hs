module UU.UUAGC (uuagc, uuagcMain) where

import Ag (uuagcLib, uuagcExe)

import System.Exit (ExitCode(..))

uuagc :: [String] -> FilePath -> IO (ExitCode, [FilePath])
uuagc = uuagcLib

uuagcMain :: IO ()
uuagcMain = uuagcExe
