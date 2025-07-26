{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module PathType where

import Control.Monad.IO.Class
import System.Directory
import System.FilePath

newtype Path (t :: PathType) = MkPath { getPath :: FilePath }
  deriving (Show, Ord, Eq)

data PathType = File | Directory
   deriving (Show, Ord, Eq)

(|/|) :: Path Directory -> Path (t :: PathType) -> Path (t :: PathType)
MkPath x |/| MkPath y = MkPath (x </> y)

mkPath :: MonadIO m => FilePath -> (Path File -> m a) -> (Path Directory -> m a) -> m a
mkPath path onFile onDir = do
  isDir <- liftIO (doesDirectoryExist path)
  if isDir then onDir (MkPath path) else onFile (MkPath path)

foo :: Path File
foo = MkPath "foo"

bar :: Path Directory
bar = MkPath "bar"
