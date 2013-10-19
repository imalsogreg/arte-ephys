module Arte.Common.FileUtils where

import System.Directory
import System.FilePath ((</>))
import Control.Monad.Trans.Writer.Strict

type FilePath = String
type FileExtention = String

getNonDots :: FilePath -> IO [FilePath]
getNonDots baseDir = filter notDot `liftM` getDirectoryContents 
  where notDot a = a /= "." && a /= ".."

getDirs :: FilePath -> IO [FilePath]
getDirs baseDir = getNonDots baseDir >>= filterM directoryExists
  
getRegularFiles :: FilePath -> IO [FilePath]
getRegularFiles bp = liftM2 nub (getNonDots bp) (getDirs bp)

getFilesRec :: FilePath -> FileExtention -> Int -> WriterT [FilePath] IO ()
getFilesRec baseDir extn searchDepht = do
  fs <- getRegularFiles baseDir
  tell (map (baseDir </>) . filter (extn `isPostfixOf`) $ fs)
  when (searchDepth > 0) $ do
    dirs <- getDirs baseDir
    forM_ dirs $ \d -> getFilesRec (baseDir </> d) extn (searchDepth - 1) 

getFilesByExtention :: FilePath -> FileExtention -> Int -> IO [FilePath]
getFilesByExtention = runWriterT getFilesRec