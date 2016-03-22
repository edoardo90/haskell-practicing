module Paths_sadf (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,0,1], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/edoardo/.cabal/bin"
libdir     = "/home/edoardo/.cabal/lib/sadf-0.0.1/ghc-7.6.3"
datadir    = "/home/edoardo/.cabal/share/sadf-0.0.1"
libexecdir = "/home/edoardo/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "sadf_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "sadf_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "sadf_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "sadf_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
