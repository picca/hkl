{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Hkl.Types where

import Foreign

newtype Factory = Factory (Ptr Factory) deriving (Show, Storable)

data HklGeometry
newtype Geometry = Geometry (ForeignPtr HklGeometry) deriving (Show)

-- Engine
newtype Engine = Engine (Ptr Engine) deriving (Show, Storable)

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
