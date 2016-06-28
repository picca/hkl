module Hkl.Engine
       ( enginesTrajectoryPipe
       , fromToPipe
       ) where

import Control.Monad (forever, forM_)
import Numeric.LinearAlgebra (Vector, toList)
import Pipes

import Hkl.Types

engineSetValues :: Engine -> Vector Double -> Engine
engineSetValues (Engine name ps mode) vs = Engine name nps mode
  where
    nps = zipWith set ps (toList vs)
    set (Parameter n _ range) newValue = Parameter n newValue range

fromToPipe :: Int -> Vector Double -> Vector Double -> Producer (Vector Double) IO ()
fromToPipe n from to = forM_ [0..n-1] $ \i -> yield $ vs i
  where
    vs i = from + step * fromIntegral i
    step = (to - from) / (fromIntegral n - 1)

enginesTrajectoryPipe :: Engine -> Pipe (Vector Double) Engine IO ()
enginesTrajectoryPipe e = forever $ await >>= yield . engineSetValues e
