{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Hkl.Types where

import Foreign

type Range = (Double, Double)

-- HklFactory should be private
newtype HklFactory = HklFactory (Ptr HklFactory) deriving (Show, Storable)

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
newtype HklEngine = HklEngine (Ptr HklEngine) deriving (Show, Storable)

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
