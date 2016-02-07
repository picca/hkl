{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Hkl.Types where

import Foreign
import Foreign.C

unit :: CInt
unit = 1

type Range = (Double, Double)

-- HklFactory should be private
data HklFactory
newtype Factory = Factory (Ptr HklFactory) deriving (Show, Storable)

-- HklParameter -- should be private
data HklParameter

-- Parameter
data Parameter = Parameter { parameterName :: String
                           , parameterValue ::Double
                           , parameterRange :: Range
                           } deriving (Show)

-- Geometry
data HklGeometry
newtype Geometry = Geometry (ForeignPtr HklGeometry) deriving (Show)

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
