module Paths_LambdaIntegrator (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import Data.Version (Version(..))
import System.Environment (getEnv)

version :: Version
version = Version {versionBranch = [0,1,0], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/usr/local/bin"
libdir     = "/usr/local/lib/LambdaIntegrator-0.1.0/ghc-6.12.1"
datadir    = "/usr/local/share/LambdaIntegrator-0.1.0"
libexecdir = "/usr/local/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "LambdaIntegrator_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "LambdaIntegrator_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "LambdaIntegrator_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "LambdaIntegrator_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
