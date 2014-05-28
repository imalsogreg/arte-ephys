{-# LANGUAGE DeriveDataTypeable #-}

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

type FileExtension = String

data PathMatchException = PathMatchException String
                        deriving (Typeable)

instance Show PathMatchException where
  show (PathMatchException s) = "PathMatchException: " ++ s

instance Exception PathMatchException

------------------------------------------------------------------------------
processPath :: String -> IO [FilePath]
processPath s = do
  d0 <- getCurrentDirectory
  dirs <- foldM processDirToken ["/"] dirTokens :: IO [FilePath]
  putStrLn $ "dirTokens: " ++ show dirTokens
  putStrLn $ "Dirs: " ++ show dirs
  f <- concat <$> mapM (flip filesAt fileToken) dirs
  setCurrentDirectory d0
  return f
  where
   tokens = (map BS.unpack . BS.split '/') . BS.pack $ s
   (dirTokens,fileToken) = (init tokens, last tokens)
   isEnv t = take 2 t == "$(" && last t == ')'
   filesAt :: FilePath -> String -> IO [FilePath]
   filesAt d f = do
     setCurrentDirectory d
     fs <- expandWildcard f
     filterM doesFileExist [d ++ "/" ++ x | x <- fs]
   processDirToken :: [FilePath] -> String -> IO [FilePath]
   processDirToken acc t = do
     let targets'
           | isEnv t = lookupEnv (drop 2 . init $ t) >>= \var -> case var of
             Nothing  ->
               throw . PathMatchException $
               "Missing env variable: " ++ (drop 2 . init $ t)
             Just var' -> print var' >> return [var']
           | '*' `elem` t = expandWildcard t
           | otherwise = onlyDir t
     res <- forM acc $ \thisD -> do
       setCurrentDirectory thisD
       targets <- targets'
       return $ [x++"/"++targ | x <- acc, targ <- targets]
     filterM doesDirectoryExist (concat res)
     

expandWildcard :: String -> IO [String]
expandWildcard t = do
  let (pre, _:post) = span (/= '*') t
  files <- (\\ [".",".."]) <$>
           (getDirectoryContents =<< getCurrentDirectory)
  return $ filter (\f -> isPrefixOf pre f && isSuffixOf post f) files

onlyDir :: String -> IO [String]
onlyDir t = doesDirectoryExist t >>= \b -> 
  if b then return [t]
  else throw $ PathMatchException ("No directory named " ++ t)
    
  
   
getFilesAndDirs :: FilePath -> IO ([FilePath],[FilePath])
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
                       
getFilesRec :: FilePath -> FileExtension -> Int -> WriterT [FilePath] IO ()
getFilesRec baseDir extn searchDepth = do
  (fs,dirs) <- liftIO $ getFilesAndDirs baseDir
  tell (map (baseDir </>) . filter (("." ++ extn) `isSuffixOf`) $ fs)
  when (searchDepth > 0) $
    forM_ dirs $ \d -> getFilesRec (baseDir </> d) extn (searchDepth - 1) 

getFilesByExtension :: FilePath -> Int -> FileExtension -> IO [FilePath]
getFilesByExtension p d e = execWriterT $ getFilesRec p e d
