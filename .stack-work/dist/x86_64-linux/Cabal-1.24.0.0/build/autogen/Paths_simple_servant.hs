{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_simple_servant (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
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
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/var/www/haskProj/simple-servant2/.stack-work/install/x86_64-linux/lts-7.16/8.0.1/bin"
libdir     = "/var/www/haskProj/simple-servant2/.stack-work/install/x86_64-linux/lts-7.16/8.0.1/lib/x86_64-linux-ghc-8.0.1/simple-servant-0.1.0.0-Lx0FbDzjMHG4CLLtNQJrOa"
datadir    = "/var/www/haskProj/simple-servant2/.stack-work/install/x86_64-linux/lts-7.16/8.0.1/share/x86_64-linux-ghc-8.0.1/simple-servant-0.1.0.0"
libexecdir = "/var/www/haskProj/simple-servant2/.stack-work/install/x86_64-linux/lts-7.16/8.0.1/libexec"
sysconfdir = "/var/www/haskProj/simple-servant2/.stack-work/install/x86_64-linux/lts-7.16/8.0.1/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "simple_servant_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "simple_servant_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "simple_servant_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "simple_servant_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "simple_servant_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
