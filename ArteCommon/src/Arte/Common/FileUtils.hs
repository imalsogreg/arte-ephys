module Arte.Common.FileUtils where

import System.Directory
import System.FilePath ((</>))
import Control.Monad
import Data.List
import Control.Monad.IO.Class
import Control.Monad.Trans.Writer.Strict

type FileExtension = String

getFilesAndDirs :: FilePath -> IO ([FilePath],[FilePath])
getFilesAndDirs basePath =
  let notDot = (\p -> (p /= ".") && (p /= ".."))
      fullDExists = doesDirectoryExist . (basePath </>)
      partitionM mf xs = do
        ys <- filterM mf xs
        ns <- filterM (liftM not . mf) xs
        return (ys,ns)
  in do
  fs <- liftM (filter notDot) . getDirectoryContents $ basePath
  (dirs,files) <- partitionM fullDExists fs
  return (files, dirs)                    
                       
getFilesRec :: FilePath -> FileExtension -> Int -> WriterT [FilePath] IO ()
getFilesRec baseDir extn searchDepth = do
  (fs,dirs) <- liftIO $ getFilesAndDirs baseDir
  tell (map (baseDir </>) . filter (("." ++ extn) `isSuffixOf`) $ fs)
  when (searchDepth > 0) $ do
    forM_ dirs $ \d -> getFilesRec (baseDir </> d) extn (searchDepth - 1) 

getFilesByExtension :: FilePath -> Int -> FileExtension -> IO [FilePath]
getFilesByExtension p d e = execWriterT $ getFilesRec p e d