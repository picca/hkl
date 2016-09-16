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
  show (Xpad32) = "Detector Xpad32"
  show (ZeroD) = "Detector ZeroD"

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
    w :: Int -> Double
    w i'
      | i' == chipw * 1 - 1 = bigPixelWidth
      | i' == chipw * 1 = bigPixelWidth
      | i' == chipw * 2 - 1 =bigPixelWidth
      | i' == chipw * 2 = bigPixelWidth
      | i' == chipw * 3 - 1 = bigPixelWidth
      | i' == chipw * 3 = bigPixelWidth
      | i' == chipw * 4 - 1 = bigPixelWidth
      | i' == chipw * 4 = bigPixelWidth
      | i' == chipw * 5 - 1 = bigPixelWidth
      | i' == chipw * 5 = bigPixelWidth
      | i' == chipw * 6 - 1 = bigPixelWidth
      | i' == chipw * 6 = bigPixelWidth
      | otherwise = pixel

    f :: Int -> Double
    f 0 = (w 0) / 2
    f i' = f (i' - 1) + (w (i' - 1)) / 2 + (w i') / 2

    -- height

    module' :: Int -> Int
    module' j = div j chiph

    g :: Int -> Double
    g j = pixel / 2.0 + (pixel * (fromIntegral j)) + interModule * fromIntegral (module' j)
