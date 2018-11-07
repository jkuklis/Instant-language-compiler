{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_instester (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,2] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/jk/.cabal/bin"
libdir     = "/home/jk/.cabal/lib/x86_64-linux-ghc-8.2.2/instester-0.2-AmOqhYtnUWOI8l19oeuC5x-instester"
dynlibdir  = "/home/jk/.cabal/lib/x86_64-linux-ghc-8.2.2"
datadir    = "/home/jk/.cabal/share/x86_64-linux-ghc-8.2.2/instester-0.2"
libexecdir = "/home/jk/.cabal/libexec/x86_64-linux-ghc-8.2.2/instester-0.2"
sysconfdir = "/home/jk/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "instester_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "instester_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "instester_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "instester_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "instester_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "instester_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
