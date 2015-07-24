{-|
Module      : System.Arte.FileUtils
Description : Miscellaneous file handling utilities
Copyright   : (c) Greg Hale, 2015
                  Shea Levy, 2015
License     : BSD3
Maintainer  : imalsogreg@gmail.com
Stability   : experimental
Portability : GHC, Linux
-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}

module System.Arte.FileUtils where

import Control.Applicative
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Writer.Strict
import Data.List
import qualified Data.ByteString.Char8 as BS
import Data.Typeable
import System.Directory
import System.Environment
import System.FilePath ((</>))

-- | The extension of a file name
type FileExtension = String

-- | An exception during path resolution
data PathMatchException = PathMatchException String
                        deriving (Typeable)

instance Show PathMatchException where
  show (PathMatchException s) = "PathMatchException: " ++ s

instance Exception PathMatchException

------------------------------------------------------------------------------
-- | Partition a directory's contents into files or directories
getFilesAndDirs :: FilePath -- ^ The directory to enumerate
                -> IO ([FilePath],[FilePath])
getFilesAndDirs basePath =
  let notDot p = (p /= ".") && (p /= "..")
      fullDExists = doesDirectoryExist . (basePath </>)
      partitionM mf xs = do
        ys <- filterM mf xs
        ns <- filterM (liftM not . mf) xs
        return (ys,ns)
  in do
  fs <- liftM (filter notDot) . getDirectoryContents $ basePath
  (dirs,files) <- partitionM fullDExists fs
  return (files, dirs)                    

-- | Get all files matching a certain extention in a directory, recursively,
--   in 'WriterT'
getFilesRec :: FilePath -- ^ The base directory
            -> FileExtension -- ^ The extension to look for
            -> Int -- ^ The recursion depth
            -> WriterT [FilePath] IO ()
getFilesRec baseDir extn searchDepth = do
  (fs,dirs) <- liftIO $ getFilesAndDirs baseDir
  tell (map (baseDir </>) . filter (("." ++ extn) `isSuffixOf`) $ fs)
  when (searchDepth > 0) $
    forM_ dirs $ \d -> getFilesRec (baseDir </> d) extn (searchDepth - 1) 

-- | Get all files matching a certain extention in a directory, recursively
getFilesByExtension :: FilePath -- ^ The base directory
                    -> Int -- ^ The recursion depth
                    -> FileExtension -- ^ The extension to look for
                    -> IO [FilePath]
getFilesByExtension p d e = execWriterT $ getFilesRec p e d
