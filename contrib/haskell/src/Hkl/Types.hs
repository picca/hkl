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
newtype Geometry = Geometry ( ForeignPtr HklGeometry
                            , Source
                            , [Double]
                            )
                 deriving (Show)

-- HklEngine -- should be private
data HklEngine
newtype Engine = Engine (Ptr HklEngine) deriving (Show, Storable)

-- EngineList
data HklEngineList
newtype EngineList = EngineList (ForeignPtr HklEngineList) deriving (Show)

-- Sample
data HklSample
newtype Sample = Sample (ForeignPtr HklSample) deriving (Show)

-- Detector
data DetectorType = DetectorType0D

data HklDetector
newtype Detector = Detector (ForeignPtr HklDetector) deriving (Show)
