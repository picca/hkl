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

coordinates Xpad32 (NptPoint x y) = fromList [g (round y), f' (round x), 0]
  where
    chipw = 80
    chiph = 120
    pixel = 130e-6
    bigPixelwidthFactor = 2.5
    bigPixelWidth = pixel * bigPixelwidthFactor
    interModule = 3.57e-3

    -- width
    w :: Int -> Double
    {-# INLINE w #-}
    w (!i')
      | i' == chipw - 1     = bigPixelWidth
      | i' == chipw         = bigPixelWidth
      | i' == chipw * 2 - 1 = bigPixelWidth
      | i' == chipw * 2     = bigPixelWidth
      | i' == chipw * 3 - 1 = bigPixelWidth
      | i' == chipw * 3     = bigPixelWidth
      | i' == chipw * 4 - 1 = bigPixelWidth
      | i' == chipw * 4     = bigPixelWidth
      | i' == chipw * 5 - 1 = bigPixelWidth
      | i' == chipw * 5     = bigPixelWidth
      | i' == chipw * 6 - 1 = bigPixelWidth
      | i' == chipw * 6     = bigPixelWidth
      | otherwise = pixel

    f :: Int -> Double
    {-# INLINE f #-}
    f 0 = w 0 / 2
    f (!i') = f (i' - 1) + w (i' - 1) / 2 + w i' / 2


    f' :: Int -> Double
    {-# INLINE f' #-}
    f' i'
      | i' <= chipw * 1 - 2 = pixel / 2.0 + pixel * fromIntegral i' + 0 * bigPixelWidth
      | i' == chipw * 1 - 1 = pixel / 2.0 + pixel * fromIntegral (i' - 1) + 0 * bigPixelWidth + pixel / 2 + bigPixelWidth / 2.0
      | i' == chipw * 1     = pixel / 2.0 + pixel * fromIntegral (i' - 2) + 0 * bigPixelWidth + pixel / 2 + bigPixelWidth / 2.0 + bigPixelWidth
      | i' == chipw * 1 + 1 = pixel / 2.0 + pixel * fromIntegral (i' - 3) + 2 * bigPixelWidth + pixel

      | i' <= chipw * 2 - 2  = pixel / 2.0 + pixel * fromIntegral i' + 2 * bigPixelWidth
      | i' == chipw * 2 - 1 = pixel / 2.0 + pixel * fromIntegral (i' - 1) + 2 * bigPixelWidth + pixel / 2 + bigPixelWidth / 2
      | i' == chipw * 2     = pixel / 2.0 + pixel * fromIntegral (i' - 2) + 2 * bigPixelWidth + pixel / 2 + bigPixelWidth / 2 + bigPixelWidth
      | i' == chipw * 2 + 1 = pixel / 2.0 + pixel * fromIntegral (i' - 3) + 4 * bigPixelWidth + pixel

      | i' <= chipw * 3 - 2  = pixel / 2.0 + pixel * fromIntegral  i' + 4 * bigPixelWidth
      | i' == chipw * 3 - 1 = pixel / 2.0 + pixel * fromIntegral (i' - 1) + 4 * bigPixelWidth + pixel / 2 + bigPixelWidth / 2
      | i' == chipw * 3     = pixel / 2.0 + pixel * fromIntegral (i' - 2) + 4 * bigPixelWidth + pixel / 2 + bigPixelWidth / 2 + bigPixelWidth
      | i' == chipw * 3 + 1 = pixel / 2.0 + pixel * fromIntegral (i' - 3) + 6 * bigPixelWidth + pixel

      | i' <= chipw * 4 - 2  = pixel / 2.0 + pixel * fromIntegral i' + 6 * bigPixelWidth
      | i' == chipw * 4 - 1 = pixel / 2.0 + pixel * fromIntegral (i' - 1) + 6 * bigPixelWidth + pixel / 2 + bigPixelWidth / 2
      | i' == chipw * 4     = pixel / 2.0 + pixel * fromIntegral (i' - 2) + 6 * bigPixelWidth + pixel / 2 + bigPixelWidth / 2 + bigPixelWidth
      | i' == chipw * 4 + 1 = pixel / 2.0 + pixel * fromIntegral (i' - 3) + 8 * bigPixelWidth + pixel

      | i' <= chipw * 5 - 2  = pixel / 2.0 + pixel * fromIntegral i' + 8 * bigPixelWidth
      | i' == chipw * 5 - 1 = pixel / 2.0 + pixel * fromIntegral (i' - 1) + 8 * bigPixelWidth + pixel / 2 + bigPixelWidth / 2
      | i' == chipw * 5     = pixel / 2.0 + pixel * fromIntegral (i' - 2) + 8 * bigPixelWidth + pixel / 2 + bigPixelWidth / 2 + bigPixelWidth
      | i' == chipw * 5 + 1 = pixel / 2.0 + pixel * fromIntegral (i' - 3) + 10 * bigPixelWidth + pixel

      | i' <= chipw * 6 - 2  = pixel / 2.0 + pixel * fromIntegral i' + 10 * bigPixelWidth
      | i' == chipw * 6 - 1 = pixel / 2.0 + pixel * fromIntegral (i' - 1) + 10 * bigPixelWidth + pixel / 2 + bigPixelWidth / 2
      | i' == chipw * 6     = pixel / 2.0 + pixel * fromIntegral (i' - 2) + 10 * bigPixelWidth + pixel / 2 + bigPixelWidth / 2 + bigPixelWidth
      | i' == chipw * 6 + 1 = pixel / 2.0 + pixel * fromIntegral (i' - 3) + 12 * bigPixelWidth + pixel

      | i' <= chipw * 7 - 2  = pixel / 2.0 + pixel * fromIntegral i' + 12 * bigPixelWidth
      | i' == chipw * 7 - 1 = pixel / 2.0 + pixel * fromIntegral (i' - 1) + 12 * bigPixelWidth + pixel / 2 + bigPixelWidth / 2
      | otherwise = error $ "wront coordinates" ++ show i'

    -- height

    module' :: Int -> Int
    {-# INLINE module' #-}
    module' j = div j chiph

    g :: Int -> Double
    {-# INLINE g #-}
    g j = pixel / 2.0 + (pixel * fromIntegral j) + interModule * fromIntegral (module' j)
