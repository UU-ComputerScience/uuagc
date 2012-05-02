module UU.UUAGC (uuagc, uuagcMain, compile, module Options) where

import Ag (uuagcLib, uuagcExe, compile)
import Options

import System.Exit (ExitCode(..))

uuagc :: [String] -> FilePath -> IO (ExitCode, [FilePath])
uuagc = uuagcLib

uuagcMain :: IO ()
uuagcMain = uuagcExe