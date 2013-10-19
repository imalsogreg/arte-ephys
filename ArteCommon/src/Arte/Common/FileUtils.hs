module Arte.Common.FileUtils where

import System.Directory
import System.FilePath ((</>))
import Control.Monad
import Data.List
import Control.Monad.IO.Class
import Control.Monad.Trans.Writer.Strict

type FileExtention = String

getNonDots :: FilePath -> IO [FilePath]
getNonDots baseDir = filter notDot `liftM` getDirectoryContents baseDir
  where notDot a = a /= "." && a /= ".."

getDirs :: FilePath -> IO [FilePath]
getDirs baseDir = do
  fs <- getNonDots baseDir
  filterM doesDirectoryExist (map (baseDir </>) fs)
  
getRegularFiles :: FilePath -> IO [FilePath]
getRegularFiles bp = liftM2 (\\) (getNonDots bp) (getDirs bp)

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
                       
getFilesRec :: FilePath -> FileExtention -> Int -> WriterT [FilePath] IO ()
getFilesRec baseDir extn searchDepth = do
  (fs,dirs) <- liftIO $ getFilesAndDirs baseDir
  liftIO $ print ("Searching in " ++ baseDir)
  liftIO $ forM_ fs $ print
  tell (map (baseDir </>) . filter (extn `isSuffixOf`) $ fs)
  when (searchDepth > 0) $ do
    liftIO $ print ("Dirs to search: " ++ show dirs)
    forM_ dirs $ \d -> getFilesRec (baseDir </> d) extn (searchDepth - 1) 

getFilesByExtention :: FilePath -> FileExtention -> Int -> IO [FilePath]
getFilesByExtention p e d = execWriterT $ getFilesRec p e d