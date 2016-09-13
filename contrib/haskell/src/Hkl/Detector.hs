{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}

module Hkl.Detector
       ( Detector(..)
       , coordinates
       ) where

import Hkl.PyFAI.Npt
import Numeric.LinearAlgebra

data Xpad32
data ZeroD

data Detector a where
  DetectorXpad32 :: Detector Xpad32
  DetectorZeroD :: Detector ZeroD

instance Show (Detector a) where
  show (DetectorXpad32) = "Detector Xpad32"
  show (DetectorZeroD) = "Detector ZeroD"


coordinates :: Detector a -> NptPoint -> Vector Double
coordinates DetectorXpad32 (NptPoint x y) = fromList [x, y, 0] * 130e-6
coordinates DetectorZeroD (NptPoint 0 0) = fromList [0, 0, 0]
coordinates DetectorZeroD _ = error "No coordinates in a ZeroD detecteor"
