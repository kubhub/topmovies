{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_topmovies (
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
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/homes/oh302/movie/.stack-work/install/x86_64-linux/lts-12.18/8.4.4/bin"
libdir     = "/homes/oh302/movie/.stack-work/install/x86_64-linux/lts-12.18/8.4.4/lib/x86_64-linux-ghc-8.4.4/topmovies-0.1.0.0-2gAAEunvf9nClO7nho62iz-topmovies-exe"
dynlibdir  = "/homes/oh302/movie/.stack-work/install/x86_64-linux/lts-12.18/8.4.4/lib/x86_64-linux-ghc-8.4.4"
datadir    = "/homes/oh302/movie/.stack-work/install/x86_64-linux/lts-12.18/8.4.4/share/x86_64-linux-ghc-8.4.4/topmovies-0.1.0.0"
libexecdir = "/homes/oh302/movie/.stack-work/install/x86_64-linux/lts-12.18/8.4.4/libexec/x86_64-linux-ghc-8.4.4/topmovies-0.1.0.0"
sysconfdir = "/homes/oh302/movie/.stack-work/install/x86_64-linux/lts-12.18/8.4.4/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "topmovies_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "topmovies_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "topmovies_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "topmovies_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "topmovies_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "topmovies_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
