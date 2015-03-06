module Kalman where
import qualified Data.Distributive as D
import Linear ((!*!), (!!*), (*!!), (!+!), (!-!), M44, M22, M24, M42, eye4, eye2, inv22)
import qualified Linear as L

-- Input transformation matrix
type InputTransformer = L.V4 (L.V2 Double)
makeInputTransformer :: Double -> InputTransformer
makeInputTransformer dt = L.V4  (L.V2 dt' 0)
                                (L.V2 dt 0)
                                (L.V2 0 dt')
                                (L.V2 0 dt)
    where dt' = dt^(2 :: Int)/2

-- Random acceleration variance in both x and y dimension
s_a :: Double
s_a = 0.1

-- Make the State transition matrix
type STM = L.V4 (L.V4 Double)
makeSTM :: Double -> STM
makeSTM dt = L.V4   (L.V4 1 dt 0 0)
                    (L.V4 0 1 0 0)
                    (L.V4 0 0 1 dt)
                    (L.V4 0 0 0 1)

-- Covariance generics
type Cov22 = L.M22 Double 
type Cov44 = L.M44 Double 

-- Covariance of the process noise
newtype CovProcNoise = CovProcNoise Cov44
makeCovProcNoise :: Double -> Double -> CovProcNoise
makeCovProcNoise dt s_a = CovProcNoise $ g !*! (t g) !!* s_a
  where g = makeInputTransformer dt :: InputTransformer
        t = D.distribute

-- Observation matrix (only privy to positional measures)
h :: M24 Double
h = L.V2 (L.V4 1 0 0 0)
         (L.V4 0 0 1 0)

-- Positional measurement noise variance in relation to
-- true, but hidden, animal position
r :: M22 Double
r = L.V2 (L.V2 0.2 0 )
         (L.V2 0  0.2)

-- Model state
-- state.z state estimate, [x x' y y']^T
-- state.p state error cov. matrix
type State = (L.V4 (L.V1 Double), M44 Double)

-- The initial state estimate and state error cov.
-- matrix. These should be use only once per run of the filter,
-- for the first sample.
initState :: State
initState = (initZ, initP)
initZ = L.V4 (L.V1 0)
             (L.V1 0)
             (L.V1 0)
             (L.V1 0)
initP = 10 *!! eye4 :: Cov44

-- A single dt evolution of the Kalman filter
-- For the intial step, initState (above) should be
-- used as the State input to this function
stepKalman :: Double -> State -> L.V4 Double -> State 
stepKalman dt (z,p) x = (z',p')
  where 
  
    -- Make the state transition matrix and process
    -- noise covariance matrix, both of which depend
    -- on dt
    f = makeSTM dt :: STM
    (CovProcNoise q) = makeCovProcNoise dt s_a :: CovProcNoise 
    
    -- A-priori  state estimate
    z_ = f !*! z:: L.V4 (L.V1 Double)
    
    -- Predicted a-priori state estimate convariance
    p_ = f !*! p !*! (t f) !+! q :: Cov44
      where t = D.distribute
    
    -- The inovation
    i = x !-! h !*! z_ :: L.V4 Double
    
    -- Innovation convariance
    s = h !*! p_ !*! (t h) !+! r :: Cov22
      where t = D.distribute

    -- Is s invertable?
    (z',p') = case inv22 s of
      Nothing -> (z,p)  -- When s isn't invertable, return old z & p (good idea?)
      Just sInv -> 
        let

          -- Compute optimal Kalman gain
          k = p_ !*! (t h) !*! sInv :: M42 Double
            where t = D.distribute

          -- A-posteriori state estimate
          -- This is the estimated position of the animal
          z' = z_ !*! k !*! i 

          -- a-posteriori state error cov. matrix estimate
          p' = (eye4 !-! k !*! h) !*! p_

        in (z',p')
