{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Hkl.Types where

import Numeric.Units.Dimensional.Prelude (Length, Angle)
import Foreign
import Foreign.C

unit :: CInt
unit = 1

data Range = Range Double Double -- min max
           deriving (Show)

-- HklFactory should be private
data HklFactory
newtype Factory = Factory (Ptr HklFactory) deriving (Show, Storable)

-- HklParameter -- should be private
data HklParameter

-- Lattice

data Lattice = Cubic (Length Double) -- a = b = c, alpha = beta = gamma = 90
             | Tetragonal (Length Double) (Length Double) -- a = b != c, alpha = beta = gamma = 90
             | Orthorhombic (Length Double) (Length Double) (Length Double) -- a != b != c,  alpha = beta = gamma = 90
             | Rhombohedral (Length Double) (Angle Double) -- a = b = c, alpha = beta = gamma != 90
             | Hexagonal (Length Double) (Length Double) -- a = b != c, alpha = beta = 90, gamma = 120
             | Monoclinic (Length Double) (Length Double) (Length Double) (Angle Double) -- a != b != c, alpha = gamma = 90, beta != 90
             | Triclinic (Length Double) (Length Double) (Length Double) (Angle Double) (Angle Double) (Angle Double) -- a != b != c, alpha != beta != gamma != 90
               deriving (Show)

-- Parameter
data Parameter = Parameter String Double Range -- name, value, range
               deriving (Show)

-- Source
data Source = Source Double -- wavelength
            deriving (Show)

-- Geometry
data HklGeometry
data Geometry = Geometry Source [Double]
              deriving (Show)

-- HklEngine -- should be private
data HklEngine

-- EngineList
data HklEngineList

-- Sample
data HklSample
data Sample = Sample String
            deriving (Show)


-- Detector
data DetectorType = DetectorType0D
                  deriving (Show)

data HklDetector
data Detector = Detector DetectorType
              deriving (Show)
