{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Hkl.Types where

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
newtype Sample = Sample (ForeignPtr HklSample) deriving (Show)

-- Detector
data DetectorType = DetectorType0D
                  deriving (Show)

data HklDetector
data Detector = Detector DetectorType
              deriving (Show)
