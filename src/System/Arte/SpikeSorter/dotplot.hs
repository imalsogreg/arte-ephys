module Main where

import qualified Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL.Raw
import Data.Bits ( (.|.) )
import System.Exit (exitWith, ExitCode (..))
import Control.Monad
import Statistics.Distribution.Exponential
import Statistics.Distribution
import System.Random
import Data.Ephys.Spike
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Concurrent
import System.ZMQ
import Data.Serialize
import Data.Either.Unwrap
import Graphics.GLUtil.BufferObjects
import Foreign.Ptr
import Foreign.Storable
import Data.Array.IO
import Foreign.Marshal.Array
import GHC.Float

--email questions to haskellalex2015@gmail.com :)

type TetFill = ((IOArray (Int, Int) Bool), Mem)

data Mem = Mem (Ptr Float) (BufferObject) Int deriving (Show) 

--THERE IS SOMETHING WRONG HERE IDK MAN

plotList :: [Plot]
plotList =  [Plot (0, (1/2)) (1,1) (0, 0, 0, 0)
            ,Plot ((1/3), (1/2)) (1,1) (0, 0, 1, 0)
            ,Plot ((2/3), (1/2)) (1,1) (0, 1, 0, 0)
            ,Plot (0, 0) (1,1) (0, 1, 1, 0)
            ,Plot ((1/3), 0) (1,1) (1, 0, 0, 0)
            ,Plot ((2/3), 0) (1,1) (1, 1, 0, 0)]
  
drawVBO :: Mem -> IO ()
drawVBO (Mem ptr buff size) = do
  let vxDesc = VertexArrayDescriptor 2 (Float) (0) $ plusPtr nullPtr 0
  GL.clientState VertexArray $= Enabled
  bindBuffer ArrayBuffer $= Just buff
  ptr1 <- mapBuffer ArrayBuffer ReadWrite
  case ptr1 of
    Just a -> do
              arrayPointer VertexArray $= vxDesc
              drawArrays Points 0 $ fromIntegral ((size `div` 2))
              GL.flush
              case ((size `div` 2) `mod` 600) of
                0 -> print (size `div` 2)
                otherwise -> return ()
                 
    Nothing -> error "null pointer"
  {-case size `mod` 100 of
    0 -> print size
    otherwise -> return-} 
  unmapBuffer ArrayBuffer
  bindBuffer ArrayBuffer $= Nothing
  GL.clientState VertexArray $= Disabled

  
drawVBO' :: [(Float, Float)] -> IO ()
drawVBO' ls = do
  renderPrimitive Points $ do
    forM_ ls $ (\(x, y) -> do
               vertex $ (Vertex3 ((realToFrac x) :: GLfloat) (realToFrac y) 0))
                 
  {-case size `mod` 100 of
    0 -> print size
    otherwise -> return-} 





drawArrays' :: Ptr Int -> PrimitiveMode -> ArrayIndex -> NumArrayIndices -> IO ()
drawArrays' ptr mode first num = do
  renderPrimitive mode $ do
  forM_ [0, 2 ..num] (\i -> do
                     x <- peekMem ptr (fromIntegral i)
                     y <- peekMem ptr $ fromIntegral (i+1)
                     vertex $ (Vertex3 ((realToFrac x):: GLfloat) (realToFrac y) 0)) 
                       

tetrodeView1 :: TetrodeView --test tetrode view. Future will have user input
tetrodeView1 = TetrodeView 0.9 0.7 (0,0)

lengthArray :: Int --how long the array of points to draw is. Can probably be optimized later
lengthArray = 1000000

initGL :: GLFW.Window -> IO ()
initGL win = do
  GL.shadeModel $= Smooth --enables smooth color shading
  GL.clearColor $=Color4 0 0 0 0 -- Clear the background color to black
  GL.clearDepth $= 1 -- enables  clearing of the depth buffer
  GL.depthFunc $= Just Lequal  -- type of depth test
  GL.hint GL.PerspectiveCorrection $= GL.Nicest
  (w,h) <- GLFW.getFramebufferSize win
  resizeScene win w h

resizeScene :: GLFW.FramebufferSizeCallback
resizeScene win w     0      = resizeScene win w 1 -- prevent divide by zero
resizeScene _  width height = do
  GL.viewport $= ((Position 0 0) , Size (fromIntegral width) (fromIntegral height)) --make a viewport with position at 0,0 and width x height.
  GL.matrixMode $= Projection
  GL.loadIdentity
  GL.ortho 0 1 0 1 (-1) 1 --bottom left, (0,0) top right (1,1) 
  GL.matrixMode $= Modelview 0
  GL.loadIdentity
  GL.flush
  
drawGrid :: IO()
drawGrid = renderPrimitive Lines $ do
  vertex $ (Vertex3 (0:: GLfloat) 1 0)
  vertex $ (Vertex3 (1:: GLfloat) 1 0)
  vertex $ (Vertex3 (1:: GLfloat) 1 0)
  vertex $ (Vertex3 (1::GLfloat) 0 0)
  vertex $ (Vertex3 (0::GLfloat) 0.5 0)
  vertex $ (Vertex3 (1::GLfloat) 0.5 0)
  vertex $ (Vertex3 (0.3333333::GLfloat) (0) 0)
  vertex $ (Vertex3 (0.3333333333::GLfloat) (1) 0)
  vertex $ Vertex3 (0.66666666666::GLfloat) 0 0
  vertex $ Vertex3 (0.66666666666::GLfloat) (1) 0
  vertex $ Vertex3 (0::GLfloat) 0 0
  vertex $ Vertex3 (0::GLfloat) 1 0
  vertex $ Vertex3 (0:: GLfloat) 0 0 
  vertex $ Vertex3 (1::GLfloat) 0 0
  
shutdown :: GLFW.WindowCloseCallback
shutdown win = do
  GLFW.destroyWindow win
  GLFW.terminate
  _ <- exitWith ExitSuccess
  return ()
  
keyPressed :: GLFW.KeyCallback
keyPressed win GLFW.Key'Escape _ GLFW.KeyState'Pressed _ = shutdown win --if pressed escape, gtfo
keyPressed _ _              _ _                       _  = return ()



drawSpike :: GLfloat -> RecSpike -> IO () --draw a spike passed the remaining distance (w-h in case of landscape) so you can divvy up into 4 spaces
drawSpike dist ls = do
  GL.scale ((realToFrac (dist/4))) 1 (1 :: GLfloat)  
  renderPrimitive LineStrip $ do
    sequence $ map drawIndivPointsSpike ls
  GL.scale (1/(realToFrac (dist/4))) 1 (1 :: GLfloat)  
  GL.translate $ Vector3 (dist/4) 0 0 --move over 1/4 of the leftover space, basically make 4 even sections
  
drawIndivPointsSpike :: (Double, Double) -> IO()
drawIndivPointsSpike (x,y) = do
  GL.vertex$ Vertex3 ((realToFrac (x/0.001))::GLfloat) (realToFrac (y/4e-3)) 0 
  
handleSocket :: Socket a -> IO [RecSpike] 
handleSocket s = do
  inp <- (receive s [])
  let recs = decode inp
  return $ fromRight recs
  
heightw :: Int
heightw = 480

widthw :: Int
widthw = 720

updateMem :: TVar (TetFill) -> [Float] -> TetrodeView -> IO ()
updateMem t ls view= do
  mapM_ (updateIndivPlot t ls view) plotList

updateMem' :: TVar ([(Float, Float)])  -> [Float] -> TetrodeView -> IO ()
updateMem' t ls view = mapM_ (updateIndivPlot' t ls view) plotList

updateIndivPlot' :: TVar [(Float, Float)] -> [Float] -> TetrodeView -> Plot -> IO ()
updateIndivPlot' t ls view p@(Plot (posx, posy) (idx, idy) (col1, col2, col3, col4)) = do
  ls <- readTVarIO t
  case (x, y) of
    (Nothing, Nothing) -> return ()
    (Nothing , _) -> return ()
    (_ , Nothing) -> return ()
    (Just a, Just b) -> do
      let newLS = (a, b) : ls
      --print ((a,b), p)
      atomically $ writeTVar t (newLS)
  where x = changeToFitx view posx $ (!!) ls (idx-1)
        y = changeToFity view posy $ (!!) ls (idy-1)

updateIndivPlot :: TVar TetFill -> [Float] -> TetrodeView -> Plot ->IO ()
updateIndivPlot t ls view p@(Plot (posx, posy) (idx, idy) (col1, col2, col3, col4)) = do
  (arr, mem) <- readTVarIO t
  case (x, y) of
    (Nothing, Nothing) -> return ()
    (Nothing , _) -> return ()
    (_ , Nothing) -> return ()
    (Just a, Just b) -> do
      newMem <- pokeMem mem b
      newMem2 <- pokeMem newMem a
      --print ((a,b), p)
      atomically $ writeTVar t (arr, newMem2)
    
  where x = changeToFitx view posx $ (!!) ls (idx-1)
        y = changeToFity view posy $ (!!) ls (idy-1)
  
changeToFitx :: TetrodeView -> Float -> Float -> Maybe Float
changeToFitx (TetrodeView _ h (viewx, _)) plotx val
  |val > 2e-3 || val < 6e-5 = Nothing
  |otherwise = Just $ ((val/2e-3) * (1/2) * (h) / ((fromIntegral widthw)/(fromIntegral heightw))) + (viewx) + (plotx * (h))

changeToFity :: TetrodeView -> Float -> Float -> Maybe Float
changeToFity (TetrodeView _ h (viewx, viewy)) ploty val
  |val > 2e-3 || val < 6e-5 = Nothing
  |otherwise = Just $ ((val/2e-3) * (1/2) * (h)) + (viewy) + (ploty * (h))
  
pokeMem :: Mem -> Float -> IO Mem
pokeMem m@(Mem ptr buff size) val = do
  bindBuffer ArrayBuffer $= Just buff
  ptr1 <- mapBuffer ArrayBuffer WriteOnly
  case ptr1 of
    Just a -> do
      pokeByteOff a (size*4) val
      unmapBuffer ArrayBuffer
      bindBuffer ArrayBuffer $= Nothing
      return $ incrementSize m
    Nothing -> do 
      error "nothing while poking"
      return m

incrementSize :: Mem -> Mem
incrementSize (Mem ptr buff size) = Mem ptr buff (size +1)
    
peekMem :: Ptr Int -> Int -> IO Float
peekMem ptr1  elem = do
  num <- peekByteOff ptr1 ((elem)*4) :: IO Float
  print (num, (plusPtr ptr1 ((elem)*4)), ((elem-1)*4)) 
  return num        
  
vboInit :: Int -> IO (Mem)
vboInit size = 
    let arrayType = ArrayBuffer 
    in do 
      [buff] <- genObjectNames 1
      bindBuffer arrayType $= Just buff 
      arrptr <- mallocArray size :: IO (Ptr Float)
      bufferData ArrayBuffer $= ((fromIntegral size), arrptr, DynamicDraw)
      ptr <- mapBuffer ArrayBuffer ReadWrite
      let act = case ptr of
            Just b -> do
              return $ Mem b buff 1
            Nothing -> error "null pointer during init"
      unmapBuffer ArrayBuffer
      bindBuffer ArrayBuffer $= Nothing 
      act
      
      
drawScene :: TVar (TetFill) -> [RecSpike] -> GLFW.Window-> IO ()
drawScene t recs _ = do
  glClear $ fromIntegral  $  gl_COLOR_BUFFER_BIT
                         .|. gl_DEPTH_BUFFER_BIT
  let TetrodeView w' h' (x',y') = tetrodeView1
      w = realToFrac w'
      h = realToFrac h'
      x = realToFrac x'
      y = realToFrac y'
  GL.loadIdentity
  GL.translate $ Vector3 (x) (y) (0::GLfloat)
  GL.scale (h) (h) (1.0 :: GLfloat)
  drawGrid
  GL.translate $ Vector3 (1) (1/2) (0:: GLfloat)
  GL.translate $ Vector3 (((w-h)/16)) 0 0
  sequence $ map (drawSpike (w-h)) recs
  GL.translate $ Vector3 (-(w-h)) 0 (0::GLfloat)
  GL.translate $ Vector3 (-(1 + ((w-h)/16))) (-1/2) 0
  GL.scale (realToFrac (1/h)) (1/h) (1.0 :: GLfloat)
  GL.translate $ Vector3 (-x) (-y) (0::GLfloat)
  (arr, mem) <- readTVarIO t
  drawVBO mem
  GL.flush

drawScene' :: TVar ([(Float, Float)]) -> [RecSpike] -> GLFW.Window-> IO ()
drawScene' t recs _ = do
  glClear $ fromIntegral  $  gl_COLOR_BUFFER_BIT
                         .|. gl_DEPTH_BUFFER_BIT
  let TetrodeView w' h' (x',y') = tetrodeView1
      w = realToFrac w'
      h = realToFrac h'
      x = realToFrac x'
      y = realToFrac y'
  GL.loadIdentity
  GL.translate $ Vector3 (x) (y) (0::GLfloat)
  GL.scale (h) (h) (1.0 :: GLfloat)
  drawGrid
  GL.translate $ Vector3 (1) (1/2) (0:: GLfloat)
  GL.translate $ Vector3 (((w-h)/16)) 0 0
  sequence $ map (drawSpike (w-h)) recs
  GL.translate $ Vector3 (-(w-h)) 0 (0::GLfloat)
  GL.translate $ Vector3 (-(1 + ((w-h)/16))) (-1/2) 0
  GL.scale (realToFrac (1/h)) (1/h) (1.0 :: GLfloat)
  GL.translate $ Vector3 (-x) (-y) (0::GLfloat)
  ls <- readTVarIO t
  drawVBO' ls
  GL.flush
  
  
main :: IO ()
main = withContext 1 $ \context -> do
  withSocket context Sub $ \subscriber1 -> do
    connect subscriber1 "tcp://localhost:7373"
    subscribe subscriber1 ""
    True <- GLFW.init
    GLFW.defaultWindowHints
    Just win <- GLFW.createWindow 720 480 "test" Nothing Nothing
    GLFW.makeContextCurrent (Just win)
    mem <- vboInit (10000000)
    arr <- Data.Array.IO.newArray ((0,0), (720,480)) False 
    t <- newTVarIO ((arr, mem) :: TetFill)
    --t <- newTVarIO ([]) 
    --let initrecs = [[(0,0)], [(0,0)], [(0,0)], [(0,0)]] :: [[(Double,Double)]]
    --first check
    -- select type of display mode:
    -- Double buffer
    -- RGBA color
    -- Alpha components supported
    -- Depth buffer
    GLFW.defaultWindowHints
    --second check
    -- register the function to do all our OpenGL drawing
    GLFW.setWindowRefreshCallback win (Just (drawScene t []))
    -- register the function called when our window is zed
    GLFW.setFramebufferSizeCallback win (Just resizeScene)
    -- register the function called when the keyboard is pressed.
    GLFW.setKeyCallback win (Just keyPressed)
    GLFW.setWindowCloseCallback win (Just shutdown)
    -- initialize our window.
    initGL win
    -- start event processing engine
    forever $ do
      recs <- handleSocket subscriber1
      let peaks = map double2Float (getLargest recs)
      updateMem t peaks tetrodeView1
      GLFW.pollEvents
      drawScene t recs win --draw the scene
      GLFW.swapBuffers win

  
getLargest :: [RecSpike] -> [Double] --take a list of RecSpikes and get the largest value to plot (we plot the max height of the spikes)
getLargest recs = map maximum (sepLists recs)