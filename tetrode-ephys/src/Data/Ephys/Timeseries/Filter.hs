module Data.Ephys.Timeseries.Filter where

-- Order of constructor arguments supposed to match natural
-- language for specifying filters, e.g. "4th order Lowpass Eliptical filter"
data FilterSpec = FilterSpec Sharpness Response Family
                deriving (Eq, Show)

data Response = LowPass  Double
              | HighPass Double
              | BandPass Double Double
              | BandStop Double Double
              | Notch    Double
              deriving (Eq, Show)

data Sharpness = Order Integer
               | PassStopRipple Double Double
               deriving (Eq, Show)

data Family = Butterworth
            | Elliptic
            deriving (Eq, Show)
