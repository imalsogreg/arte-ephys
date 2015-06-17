{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random
import Data.Map as M (Map, fromList, toList, (!), keys)
import Codec.Picture.Types
import Control.Monad.Primitive
import Control.Monad
import Graphics.Gloss.Juicy
import Control.Concurrent.Async
import Control.Concurrent.STM
import Pipes
import Pipes.RealTime
import System.Environment
import System.Directory
import Data.Ephys.OldMWL.ParseSpike
import Data.Time.Clock
import Data.Monoid
import Data.Ephys.Spike
import Data.ByteString.Lazy (toStrict)
import qualified Data.ByteString as BS (drop)
import Codec.Picture.Bitmap
import Control.Applicative
import Data.Vector as V ((!))
import Codec.BMP
import Data.Binary
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Distributed.Process.Node (LocalNode, localNodeId, initRemoteTable, runProcess)
import Control.Distributed.Process
import Control.Distributed.Process.Closure
import Control.Distributed.Process
import Data.Typeable
import System.Arte.FileUtils
import qualified Data.List as L  
import qualified Control.Concurrent as C
import qualified Pipes.Prelude as PP


type TempData = TrodeSpike
  
type MultTetrodes = Map Int TotalPlots

type TotalPlots = Map (Int, Int) Plot

type MutImage = MutableImage (PrimState IO) PixelRGBA8
type FrozImage = Image PixelRGBA8

data Plot = Plot { image:: MutImage
                 , id   :: Int       
                 }
            
data World = World { tets        :: MultTetrodes
                   , spikeChan   :: TChan TempData --TrodeSpike
                   , time        :: Double
                   , currchanX   :: Int
                   , currchanY   :: Int
                   , currTet     :: Int
                   }

data SlaveControllerMsg
   = SlaveTerminate
   | RedirectLogsTo ProcessId ProcessId
  deriving (Typeable, Show)

instance Binary SlaveControllerMsg where
  put SlaveTerminate = putWord8 0
  put (RedirectLogsTo a b) = do putWord8 1; put (a,b)
  get = do
    header <- getWord8
    case header of
      0 -> return SlaveTerminate
      1 -> do (a,b) <- get; return (RedirectLogsTo a b)
      _ -> fail "SlaveControllerMsg.get: invalid"

data RedirectLogsReply
  = RedirectLogsReply ProcessId Bool
  deriving (Typeable, Show)

instance Binary RedirectLogsReply where
  put (RedirectLogsReply from ok) = put (from,ok)
  get = do
    (from,ok) <- get
    return (RedirectLogsReply from ok)

green'   = PixelRGBA8 255 0 255 0
blue'    = PixelRGBA8 0 255 255 0
red'     = PixelRGBA8 0 0 255 255
white'   = PixelRGBA8 255 255 255 255
yellow'  = PixelRGBA8 255 0 255 255
cyan'    = PixelRGBA8 255 255 255 0
magenta' = PixelRGBA8 0 255 255 255 



mapPlots :: TotalPlots -> FilePath -> (Int, TotalPlots)
mapPlots tp fn = ((trim fn), tp)


getRandomNumTup :: IO [Double]
getRandomNumTup = do
  a <-getStdRandom (randomR (0, 699))
  b <-getStdRandom (randomR (0, 699))
  c <-getStdRandom (randomR (0, 699))
  d <-getStdRandom (randomR (0, 699))
  return [a,b,c,d]

------------------------Initializing stuff-------------------------
listOfKeys :: [(Int, Int)]
listOfKeys = [(x,y) | x <- [0..3], y <- [0..3], x < y]

myMutable :: IO MutImage -- <Checked>
myMutable = createMutableImage 500 500 (PixelRGBA8 255 0 0 255) --creates a new Mutable Image that's 720 by 480 and is all black

myFrozen :: IO FrozImage
myFrozen = do
  mut <- myMutable
  freezeImage mut

initWorld :: TChan TempData -> MultTetrodes -> World --initializes the world <Checked>
initWorld c mt = World mt c 4492 0 1 7

initPlots :: IO TotalPlots -- <Checked>
initPlots = do
  let listOfIOImages (x,y) = x `seq` y `seq` createMutableImage 700 700 (PixelRGBA8 0 (fromIntegral y) 255 (fromIntegral x))
      toPlot im num = Plot im num 
  lsIm <- forM listOfKeys listOfIOImages 
--sequence $ listOfIOImages [] --Turn that list of IO Mutable Images into a list of images
  let plots = zipWith toPlot lsIm ([0..5] :: [Int])  
  return $ (fromList (zip listOfKeys (plots)))-- zip the keys with mutable images and then turn them all into a map. Return the map in the IO monad.

trim ::FilePath -> Int
trim fn = read $ take 2 $ drop ((length fn) - 7) fn :: Int
      


-----------------------Conversion Stuff-------------------------------
imToPic :: MutImage -> IO Picture --this function has been <Checked>.
imToPic mutim = do
  im <- freezeImage mutim --changes MutableImage to Image
  let bString = encodeBitmap im
  return $ scale (1) (-1) $ bitmapOfByteString 700 700 (BS.drop (54) $ toStrict bString) False

toPointList :: TrodeSpike -> (Int, [Double]) -- <Checked>
toPointList s = ((spikeTrodeName s),
                ((realToFrac $ (V.!) (spikeAmplitudes s) 0):
                 (realToFrac $ (V.!) (spikeAmplitudes s) 1):
                 (realToFrac $ (V.!) (spikeAmplitudes s) 2):
                 (realToFrac $ (V.!) (spikeAmplitudes s) 3):[]))



toPointList' :: TrodeSpike -> [Double]
toPointList' s = [1,2,3,4]


scaleFac :: Double
scaleFac = 2e6
                

-------------------Gloss stuff-----------------------------

initGloss :: TChan TempData -> IO()
initGloss c = do
  --(fn:_) <- getArgs
  
  --c <- newTChanIO
  --_ <- async. runEffect $ produceTrodeSpikesFromFile fn 16
    --   >-> relativeTimeCat (\s -> spikeTime s - 4492)
      -- >-> cToTChan c-}
  files <- getFilesByExtension "/home/chennosaurus/Programming/haskellStuff" 10 "tt"
  t0 <- getCurrentTime
  plots <- initPlots
  let tets = fromList $ map (mapPlots plots) files  
  let d = InWindow "cool window" (700, 700) (0,0)
  playIO d blue 300 (initWorld c tets) (drawWorld) handleInp $ step t0
  


drawWorld :: World -> IO Picture --changes from world to actual picture
drawWorld (World tets c _ chanx chany chanTet) = do
  let currTet =  (M.!) tets (chanTet)
  let currPlot = (M.!) currTet (chanx, chany)
      im       = image currPlot
  --putStrLn $ "drawing to" ++ (show (chanx, chany))
  imToPic (im)  

----------------------update stuff------------------------------


step :: UTCTime -> Float -> World -> IO World
step t0 _ w = do
  tNext <- getExperimentTime t0 4492
  let c = spikeChan w --do not delete pls. needed.
  emp <- (atomically $ isEmptyTChan c)
  if (emp == False) 
    then do 
      spike <- atomically $ flushChan c
      --print spike 
      mapM_ (updateTet (tets w)) (map toPointList spike)
      return w { time = tNext}
    else return w {time = tNext}

updateTet :: MultTetrodes -> (Int, [Double]) -> IO ()
updateTet mt (key, ls) = do
  let tplot = (M.!) mt key
  print key
  updateBMPs tplot ls

updateBMPs :: TotalPlots -> [Double] -> IO ()
updateBMPs plots updatels = do
  let ls = toList plots
  mapM_ (indivPlots (updatels)) ls


indivPlots :: [Double] -> ((Int, Int), Plot) -> IO () --not a problem here
indivPlots ls ((chanx, chany), (plot@(Plot _ id))) = do
  let mutIm = image plot
  if ((length ls) == 0)
    then return ()
    else do
    let x     = ceiling $ scaleFac * (!!) ls chanx
        y     = ceiling $ scaleFac * (!!) ls chany
    --putStrLn $ "updating to" ++ (show (chanx, chany)) ++ (show (x,y))
    if ((x > 0) && (y > 0)) -- && (x < 700) && (y < 700))
      then do
           writePixel mutIm x y color
           indivPlots (drop 4 ls) ((chanx, chany), plot)
      else indivPlots (drop 4 ls) ((chanx, chany), plot)
  where color = case id of 0 -> green'
                           1 -> blue'
                           2 -> red'
                           3 -> magenta'
                           4 -> yellow'
                           5 -> cyan'
                           
--------------------------------TChan stuff--------------------
                
cToTChan :: TChan TrodeSpike -> Consumer TrodeSpike IO() --consumer that forever writes the spikes to the tChan
cToTChan c =  forever $ do
                --liftIO $ putStrLn "adding"
                a <-await
                --liftIO $ putStrLn "done awaiting"
                liftIO . atomically $ writeTChan c a
                --liftIO $ putStrLn "added"

flushChan :: TChan TrodeSpike -> STM [TrodeSpike]
flushChan c = go []
  where go acc = do
          emp <- isEmptyTChan c
          if emp
            then return $ reverse acc
            else do
             e <- readTChan c
             go (e:acc)
{-
flushChan' :: TChan TempData -> STM [[Double]]
flushChan' c = go []
  where go acc = do
          emp <- isEmptyTChan c
          if emp
            then return $ reverse acc
            else do
             e <- readTChan c
             go (e:acc)  
-}

----------------------------time stuff--------------------------------

getExperimentTime :: UTCTime -> Double -> IO Double
getExperimentTime t0 et0 =
  (et0 +) . realToFrac . flip diffUTCTime t0 <$> getCurrentTime



-----------------------Input stuff---------------

handleInp :: Event -> World -> IO World
handleInp (EventKey (MouseButton b) Up _ _) w@(World _ _ _ x y _ )
{-
  | b == LeftButton = 
    return $ w {currchanX = (currchanX w + 1) `mod` 4}
  | b == RightButton =
      return $ w { currchanY = (currchanY w + 1) `mod` 4}
-}
  | b == LeftButton = do
    let (newX, newY) = nextChannel x y
    return $ w {currchanX = newX, currchanY = newY}
  | otherwise = return w
handleInp (EventKey (SpecialKey KeySpace) Down _ _) w@(World tets _ _ _ _ t) =  do
  let k = M.keys tets
  let Just ind = L.elemIndex t k
  return $ w {currTet = (!!) k (ind + 1)}

handleInp _ w = return w


nextChannel :: Int -> Int -> (Int, Int)
nextChannel x y= case (x,y) of (0,1) -> (0,2)
                               (0,2) -> (0,3)
                               (0,3) -> (1,2)
                               (1,2) -> (1,3)
                               (1,3) -> (2,3)
                               (2,3) -> (0,1)
                               otherwise -> (0,1)
  
-----------------------------cloud stuff--------------------------------

sender :: TChan TempData -> FilePath -> Process ()
sender c file = do
  liftIO $ C.forkIO $ runEffect $ produceTrodeSpikesFromFile file (trim file) 
       >-> relativeTimeCat (\s -> spikeTime s -  4492) --4491)
       >-> cToTChan c
  liftIO $ print "done with producing"
  return ()

sender' :: Process ()
sender' = do
  liftIO $ putStrLn "expecting sendport"
  myId <- getSelfPid
  liftIO  $ print myId
  s <- expect :: Process (SendPort TempData)
  liftIO $ putStrLn "got a sendport"
  c <- liftIO $ newTChanIO
  liftIO $ C.forkIO $ runEffect $ produceTrodeSpikesFromFile "/home/chennosaurus/Programming/haskellStuff/112812clip2/1628/1628.tt" 16 
       >-> relativeTimeCat (\s -> spikeTime s -  4492) --4491)
       >-> cToTChan c
  liftIO $ putStrLn "past the cToTChan"
  forever $ do
           sendDat s c
  
sendDat :: SendPort TempData -> TChan TrodeSpike -> Process () 
sendDat s c= do
  emp <- inp $ isEmptyTChan c
  if emp
     then do
        --liftIO $ putStrLn "empty tchan"
       return ()
     else do
       e <- inp $ readTChan c
       sendChan s e
       sendDat s c
  where inp = \a -> liftIO.atomically $ a

receiver :: ProcessId -> Process()
receiver id = do
  (sendCh, receiveCh) <- newChan :: Process (SendPort (TempData), ReceivePort (TempData))
  liftIO $ print id
  send id sendCh
  liftIO $ putStrLn "sent port"
  c <- liftIO $ newTChanIO
  receiveData c receiveCh
  liftIO $ C.forkIO $ initGloss c
  forever $ do
    receiveData c receiveCh

receiveData :: TChan TempData -> ReceivePort (TempData) -> Process ()
receiveData c port = do
  dat <- receiveChan port
  liftIO.atomically $ writeTChan c dat
  --DISPLAYPLOTS
 
go :: Process ()
go = do
  (typ, senId) <- expect :: Process (String, ProcessId)                
  case typ of
    "receiver" -> receiver senId
    _ -> do
         liftIO $ print "finding files"
         fn <- liftIO $ getFilesByExtension "/home/chennosaurus" 10 "tt"
         liftIO $ print "found"
         liftIO $ print fn
         s <- expect :: Process (SendPort TempData)
         liftIO $ putStrLn "got a sendport"
         c <- liftIO $ newTChanIO
         sequence $ map (sender c) fn 
         --sequence $ map (sender c) fn
         forever $ do
           sendDat s c

go' :: Process ()
go' = do
  id <- getSelfPid
  liftIO $ print id
  (typ, senId) <- expect :: Process (String, ProcessId) 
  --typ <- readTVarIO                
  case typ of
    "receiver" -> receiver senId
    _ -> sender'
  

remotable ['go, 'go']

master :: [NodeId] -> Process ()
master [] = liftIO $ putStrLn "no slaves"
master [sender' , receiver'] = do
  a <- spawn  sender'  $(mkStaticClosure 'go')
  b <- spawn receiver' $ $(mkStaticClosure('go'))
 -- b <- spawn receiver' $ $(mkClosure('receiver)) a
  whereisRemoteAsync sender' "dotplot"
  WhereIsReply str (Just sendID) <- expect :: Process (WhereIsReply)
  liftIO $ print (sendID, a)
  whereisRemoteAsync receiver' "dotplot"
  WhereIsReply str2 (Just dispID) <- expect :: Process (WhereIsReply)
  liftIO $ print (dispID, b)
  send a ("sender", a)
  send b ("receiver", a)
  return ()
master _ = liftIO $ print "dafuq"


main :: IO () 
main = do
  [serverType, port] <- getArgs
  t <- newTVarIO " "
  case serverType of
    "master" -> do  
       backend <- initializeBackend "localhost" port rtable
       nod <- newLocalNode backend
       pers <- findPeers backend 10000
       print pers
       runProcess nod $ master (take 2 pers)
       return ()
    "dotplot" -> do
       backend <- initializeBackend "localhost" port rtable
       atomically $ writeTVar t serverType
       startSlave' "dotplot" backend
    "getSpikes" -> do
       backend <- initializeBackend "localhost" port rtable
       atomically $ writeTVar t serverType
       startSlave' "dotplot" backend
   where
     rtable :: RemoteTable
     rtable = __remoteTable initRemoteTable

-----------------------------useless stuff for final----------

---------------------------utils-----------------------------
startSlave' :: String -> Backend -> IO ()
startSlave' str backend = do
  node <- newLocalNode backend
  runProcess node (stay str node)

stay :: String -> LocalNode ->Process ()
stay str node = do
  pid <- getSelfPid
  let nodeID = localNodeId node
  registerRemoteAsync nodeID str pid
  RegisterReply str bool <- expect :: Process (RegisterReply)
  () <- expect
  return ()