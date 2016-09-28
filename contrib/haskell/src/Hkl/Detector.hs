{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BangPatterns #-}

module Hkl.Detector
       ( Detector(..)
       , coordinates
       ) where

import Hkl.PyFAI.Npt
import Numeric.LinearAlgebra

data Xpad32
data ZeroD

data Detector a where
  Xpad32 :: Detector Xpad32
  ZeroD :: Detector ZeroD

instance Show (Detector a) where
  show Xpad32 = "Detector Xpad32"
  show ZeroD = "Detector ZeroD"

coordinates :: Detector a -> NptPoint -> Vector Double

-- | ZeroD

coordinates ZeroD (NptPoint 0 0) = fromList [0, 0, 0]
coordinates ZeroD _ = error "No coordinates in a ZeroD detecteor"

-- | Xpad32

coordinates Xpad32 (NptPoint x y) = fromList [g (round y), f (round x), 0]
  where
    chipw = 80
    chiph = 120
    pixel = 130e-6
    bigPixelwidthFactor = 2.5
    bigPixelWidth = pixel * bigPixelwidthFactor
    interModule = 3.57e-3

    -- width
    f :: Int -> Double
    f 0 = pixel / 2
    f 1 = pixel * 3 / 2
    f i'
      | idxInChip == 0  = pixel * (fromIntegral i' + 3 * fromIntegral chip - 1 / 4)
      | idxInChip <= 78 = pixel * (fromIntegral i' + 3 * fromIntegral chip + 1 / 2)
      | idxInChip == 79 = pixel * (fromIntegral i' + 3 * fromIntegral chip + 5 / 4)
      | otherwise = error $ "wront coordinates" ++ show i'
      where
        (chip, idxInChip) = divMod i' chipw

    -- height
    g :: Int -> Double
    g j = pixel / 2.0 + (pixel * fromIntegral j) + interModule * fromIntegral module'
      where
        module' :: Int
        module' = div j chiph
