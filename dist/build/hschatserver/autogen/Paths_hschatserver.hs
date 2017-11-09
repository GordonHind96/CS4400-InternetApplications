{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_hschatserver (
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

bindir     = "C:\\Users\\gordo\\AppData\\Roaming\\cabal\\bin"
libdir     = "C:\\Users\\gordo\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-8.2.1\\hschatserver-0.1.0.0-IpJwE6Fh5zCKsDqFsKVDdd"
dynlibdir  = "C:\\Users\\gordo\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-8.2.1"
datadir    = "C:\\Users\\gordo\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-8.2.1\\hschatserver-0.1.0.0"
libexecdir = "C:\\Users\\gordo\\AppData\\Roaming\\cabal\\hschatserver-0.1.0.0-IpJwE6Fh5zCKsDqFsKVDdd\\x86_64-windows-ghc-8.2.1\\hschatserver-0.1.0.0"
sysconfdir = "C:\\Users\\gordo\\AppData\\Roaming\\cabal\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "hschatserver_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "hschatserver_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "hschatserver_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "hschatserver_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "hschatserver_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "hschatserver_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
