module Paths_myLittleScheme (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/DrRuckus/Programming/Haskell/myLittleScheme/.stack-work/install/x86_64-osx/lts-6.5/7.10.3/bin"
libdir     = "/Users/DrRuckus/Programming/Haskell/myLittleScheme/.stack-work/install/x86_64-osx/lts-6.5/7.10.3/lib/x86_64-osx-ghc-7.10.3/myLittleScheme-0.1.0.0-EQeDMR3H1XDGpIrUqj8KmJ"
datadir    = "/Users/DrRuckus/Programming/Haskell/myLittleScheme/.stack-work/install/x86_64-osx/lts-6.5/7.10.3/share/x86_64-osx-ghc-7.10.3/myLittleScheme-0.1.0.0"
libexecdir = "/Users/DrRuckus/Programming/Haskell/myLittleScheme/.stack-work/install/x86_64-osx/lts-6.5/7.10.3/libexec"
sysconfdir = "/Users/DrRuckus/Programming/Haskell/myLittleScheme/.stack-work/install/x86_64-osx/lts-6.5/7.10.3/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "myLittleScheme_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "myLittleScheme_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "myLittleScheme_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "myLittleScheme_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "myLittleScheme_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
