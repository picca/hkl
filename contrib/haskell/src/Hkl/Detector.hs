{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}

module Hkl.Detector
       ( Detector(..)
       , coordinates
       ) where

import Hkl.PyFAI.Npt
import Numeric.LinearAlgebra

data ImXpadS140
data Xpad32
data ZeroD

data Detector a where
  ImXpadS140 :: Detector ImXpadS140
  Xpad32 :: Detector Xpad32
  ZeroD :: Detector ZeroD

instance Show (Detector a) where
  show ImXpadS140 = "Detector ImXpadS140"
  show Xpad32 = "Detector Xpad32"
  show ZeroD = "Detector ZeroD"

-- | Xpad Family

type Gap = Double
type PixelSize = Double
type Width = Int
type Index = Int

xpadLine :: PixelSize -> Width -> Index -> Double
xpadLine s _ 0 = s / 2
xpadLine s _ 1 = s * 3 / 2
xpadLine s w i'
      | idx == 0       = s * (fromIntegral i' + 3 * fromIntegral c - 1 / 4)
      | idx <= (w - 2) = s * (fromIntegral i' + 3 * fromIntegral c + 1 / 2)
      | idx == (w - 1) = s * (fromIntegral i' + 3 * fromIntegral c + 5 / 4)
      | otherwise = error $ "wront coordinates" ++ show i'
      where
        (c, idx) = divMod i' w

xpadLineWithGap :: PixelSize -> Width -> Gap -> Index -> Double
xpadLineWithGap s w g i' = s / 2 + (s * fromIntegral i') + g * fromIntegral (div i' w)

interp :: (Int -> Double) -> Double -> Double
interp f p
  | p0 == p1  = f p0
  | otherwise = (p - fromIntegral p0) * (f p1 - f p0) + f p0
  where
    p0 :: Int
    p0 = floor p

    p1 :: Int
    p1 = ceiling p

-- compute the coordinated at a given point

coordinates :: Detector a -> NptPoint -> Vector Double
coordinates ZeroD (NptPoint 0 0) = fromList [0, 0, 0]
coordinates ZeroD _ = error "No coordinates in a ZeroD detecteor"

coordinates ImXpadS140 (NptPoint x y) =
  fromList [ interp (xpadLine 130e-6 120) y
           , interp (xpadLine 130e-6  80) x
           , 0
           ]

coordinates Xpad32 (NptPoint x y) =
  fromList [ interp (xpadLineWithGap 130e-6 120 3.57e-3) y
           , interp (xpadLine        130e-6 80) x
           , 0]
