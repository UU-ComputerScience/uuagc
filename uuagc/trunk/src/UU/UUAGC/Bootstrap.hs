module UU.UUAGC.Bootstrap (uuagcBootstrap, uuagcBootstrapMain) where

import Ag (uuagcLib, uuagcExe)

import System.Exit (ExitCode(..))

uuagcBootstrap :: [String] -> FilePath -> IO (ExitCode, [FilePath])
uuagcBootstrap = uuagcLib

uuagcBootstrapMain :: IO ()
uuagcBootstrapMain = uuagcExe