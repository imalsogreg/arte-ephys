module Data.Ephys.OldMWL.ParseClusterFile where

import Data.Ephys.Cluster
import Data.Ephys.OldMWL.Parse
import Data.Ephys.OldMWL.ParseSpike
import Data.Ephys.OldMWL.FileInfo

import Data.Map
import qualified Data.Text as T
import Text.Parsec
import Control.Monad (liftM, replicateM)
import Control.Applicative ((*>),(<*>),(<$>))
import Control.Lens hiding (noneOf)

getClusters :: FilePath -> FilePath -> IO (Either String (Map Int ClusterMethod))
getClusters clustFile waveformFile = do
  eClusts <- parseClusters `liftM` readFile clustFile :: IO (Either ParseError (Map Int ClusterMethod))
  eFI     <- getFileInfo waveformFile :: IO (Either String FileInfo)
  case (eClusts,eFI) of
    (Right clusts, Right fi) -> 
      return $ Right (Data.Map.map (clusterMWLToVolts (fileGains fi)) clusts)
    _ -> return $ Left "Unsuccessful at getting clusters and adjusting their gains."

boundMWLToVolts :: [Double] -> CartBound -> CartBound
boundMWLToVolts gains b@(CartBound _ _ pts) =
  let gainX = gains !! (b ^. cartXChan) :: Double
      gainY = gains !! (b ^. cartYChan) in
  b { _cartPolygon = Prelude.map (\(x,y) -> (mwlUnitsToVoltage gainX x, mwlUnitsToVoltage gainY y)) pts }
      

clusterMWLToVolts :: [Double] -> ClusterMethod -> ClusterMethod
clusterMWLToVolts gains (ClustCartBound cb) =
  ClustCartBound (boundMWLToVolts gains cb)
clusterMWLToVolts gains (ClustIntersection bs) =
  ClustIntersection (Prelude.map (clusterMWLToVolts gains) bs)
clusterMWLToVolts _ _ =
  error "Impossible case - MWL clusters are all intersections of cartesian bounds"

parseClusters :: String -> Either ParseError (Map Int ClusterMethod)
parseClusters contents = parse pClusterFile "Cluster file" contents

foldList :: [(Int, CartBound)] -> Map Int ClusterMethod
foldList bs = Prelude.foldl appendByKey empty bs

appendByKey :: Map Int ClusterMethod -> (Int, CartBound) -> Map Int ClusterMethod
appendByKey m (k,v) = case Data.Map.lookup k m of
  Nothing                     -> insert k (ClustIntersection [ClustCartBound v]) m
  Just (ClustIntersection vs) -> insert k (ClustIntersection ((ClustCartBound v:vs))) m
  Just _                      -> error "Impossible case - MWL clusts are intersections"

pClusterFile :: Parsec String () (Map Int ClusterMethod)
pClusterFile = do
  _ <- many pHeader
  assoc <- many pProjectionPoly
  eof
  return $ foldList  assoc

pHeader :: Parsec String st ()
pHeader = do
  _ <- char '%'
  _ <- many (noneOf "\n")
  _ <- newline
  return ()

pProjectionPoly :: Parsec String () (Int, CartBound)
pProjectionPoly = do
  _ <- newline
  clustId <- read `liftM` many1 digit
  _ <- newline
  proj1 <- read `liftM` many1 digit
  _ <- char '\t'
  proj2 <- read `liftM` many1 digit
  _ <- newline
  _ <- many (noneOf "\n\t")
  _ <- newline
  _ <- many (noneOf "\n\t")
  _ <- newline
  nPoint <- read `liftM` many1 digit
  _ <- many (noneOf "\n")
  _ <- newline
  pts <- replicateM nPoint $ do
    x <- pDouble
    _ <- char '\t'
    y <- pDouble
    _ <- newline
    return (x,y)
  -- Projection indices in MWL are 1-indexed.  We want 0-indexed.
  return $ (clustId, CartBound (proj1 - 1) (proj2 - 1) pts)
  
pNumber :: Parsec String () String
pNumber = many1 digit
  
pInt :: Parsec String () String
pInt = pPlus <|> pMinus <|> pNumber
  where pPlus  = char '+' *> pNumber
        pMinus = (:) <$> char '-' <*> pNumber 

pDouble :: Parsec String () Double
pDouble = rd <$> ((++) <$> pInt <*> pDecimal)
  where rd = read :: String -> Double
        pDecimal = option "" ((:) <$> char '.' <*> pNumber)

-- "path/to/0224.tt" -> "path/to/cbfile-run"
cbNameFromTTPath :: String -> FilePath -> T.Text
cbNameFromTTPath cbFileName ttPath = T.pack . (++ cbFileName) .
                              reverse . dropWhile (/= '/') . reverse $ ttPath
