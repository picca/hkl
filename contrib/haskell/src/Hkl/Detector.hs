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
      | idxInChip == 0  = pixel / 2 + pixel * fromIntegral (i' - (chip - 1) * 2 - 2) + fromIntegral (chip - 1) * 2 * bigPixelWidth + pixel / 2 + bigPixelWidth / 2 + bigPixelWidth
      | idxInChip == 1  = pixel / 2 + pixel * fromIntegral (i' - (chip - 1) * 2 - 3) + fromIntegral (chip * 2) * bigPixelWidth + pixel
      | idxInChip <= 78 = pixel / 2 + pixel * fromIntegral (i' - (chip * 2) - 0) + fromIntegral (chip * 2) * bigPixelWidth
      | idxInChip == 79 = pixel / 2 + pixel * fromIntegral (i' - (chip * 2) - 1) + fromIntegral (chip * 2) * bigPixelWidth + pixel / 2 + bigPixelWidth / 2
      | otherwise = error $ "wront coordinates" ++ show i'
      where
        (chip, idxInChip) = divMod i' chipw

    -- height
    g :: Int -> Double
    g j = pixel / 2.0 + (pixel * fromIntegral j) + interModule * fromIntegral (module' j)
      where
        module' = div j chiph
