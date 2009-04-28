module Paths_uu_parsinglib (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import Data.Version (Version(..))
import System.Environment (getEnv)

version :: Version
version = Version {versionBranch = [2,0,0], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/usr/local/bin"
libdir     = "/usr/local/lib/uu-parsinglib-2.0.0/ghc-6.10.1"
datadir    = "/usr/local/share/uu-parsinglib-2.0.0"
libexecdir = "/usr/local/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "uu_parsinglib_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "uu_parsinglib_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "uu_parsinglib_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "uu_parsinglib_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
