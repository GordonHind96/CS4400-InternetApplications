{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_charserver (
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
libdir     = "C:\\Users\\gordo\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-8.2.1\\charserver-0.1.0.0-KygRJmVjlSFCJKb5NQhQ6H"
dynlibdir  = "C:\\Users\\gordo\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-8.2.1"
datadir    = "C:\\Users\\gordo\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-8.2.1\\charserver-0.1.0.0"
libexecdir = "C:\\Users\\gordo\\AppData\\Roaming\\cabal\\charserver-0.1.0.0-KygRJmVjlSFCJKb5NQhQ6H\\x86_64-windows-ghc-8.2.1\\charserver-0.1.0.0"
sysconfdir = "C:\\Users\\gordo\\AppData\\Roaming\\cabal\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "charserver_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "charserver_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "charserver_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "charserver_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "charserver_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "charserver_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
