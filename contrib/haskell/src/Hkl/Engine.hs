module Hkl.Engine
    ( enginesTrajectory
    , fromTo )
    where
      
import Data.List (transpose)
import Hkl.Types

linspace :: Int -> Double -> Double -> [Double]
linspace n a b = add a $ scale s [0 .. fromIntegral n-1]
    where scale _s = map (_s *)
          add _a = map (_a +)
          s = (b-a)/fromIntegral (n-1)

fromTo :: Int -> [Double] -> [Double] -> Trajectory
fromTo n f t = transpose (zipWith (linspace n) f t)

engineSetValues :: Engine -> [Double] -> Engine
engineSetValues (Engine name ps mode) vs = Engine name nps mode
    where nps = zipWith set ps vs
          set (Parameter n _ range) newValue =  Parameter n newValue range


enginesTrajectory :: Engine -> Trajectory -> [Engine]
enginesTrajectory e = map (engineSetValues e)
