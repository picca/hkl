module Hkl.Types ( Beamline(..)
                 , Mode(..)
                 , Engine(..)
                 , Factory(..)
                 , Geometry(..)
                 , Lattice(..)
                 , Sample(..)
                 , Source(..)
                 , Trajectory
                   -- factory
                 , factoryFromString
                   -- hdf5
                 , H5Path
                 , ExtendDims(..)
                 , DataItem(..)
                 , module X
                 ) where

import Hkl.Types.Parameter as X

import Data.Vector.Storable (Vector)
import Numeric.Units.Dimensional.Prelude (Length, Angle)

-- | Beamline

data Beamline = Diffabs | Sixs

instance Show Beamline where
  show Diffabs = "diffabs"
  show Sixs = "sixs"

-- | Engine

data Mode
  = Mode
    String -- ^ name
    [Parameter] -- ^ parameters of the @Mode@
  deriving (Show)

data Engine
  = Engine
    String -- ^ name
    [Parameter] -- ^ pseudo axes values of the @Engine@
    Mode -- ^ current Mode
  deriving (Show)

-- | Factory

data Factory = K6c | Uhv | MedH | MedV

instance Show Factory where
  show K6c = "K6C"
  show Uhv = "ZAXIS"
  show MedH = "todo"
  show MedV = "todo"

factoryFromString :: String -> Factory
factoryFromString s
  | s == "K6C"  = K6c
  | s == "ZAXIS" = Uhv
  | s == "todo" = MedH
  | s == "todo" = MedV
  | otherwise   = error $ "unknown diffractometer type:" ++ s

-- | Geometry

data Geometry = Geometry
                Factory -- ^ the type of diffractometer
                Source -- ^ source
                (Vector Double) -- ^ axes position
                (Maybe [Parameter]) -- ^ axes configuration
              deriving (Show)

-- | Lattice

data Lattice
  = Cubic -- ^ a = b = c, alpha = beta = gamma = 90
    (Length Double) -- a
  | Tetragonal -- ^ a = b != c,  alpha = beta = gamma = 90
    (Length Double) -- ^ a, b
    (Length Double) -- ^ c
  | Orthorhombic -- ^ a != b != c,  alpha = beta = gamma = 90
    (Length Double) -- ^ a
    (Length Double) -- ^ b
    (Length Double) -- ^ c
  | Rhombohedral -- ^ a = b = c, alpha = beta = gamma != 90
    (Length Double) -- ^ a, b, c
    (Angle Double) -- ^ alpha, beta, gamma
  | Hexagonal -- ^ a = b != c, alpha = beta = 90, gamma = 120
    (Length Double) -- ^ a, b
    (Length Double) -- ^ c
  | Monoclinic -- ^ a != b != c, alpha = gamma = 90, beta != 90
    (Length Double) -- ^ a
    (Length Double) -- ^ b
    (Length Double) -- ^ c
    (Angle Double) -- ^ beta
  | Triclinic -- ^ a != b != c, alpha != beta != gamma != 90
    (Length Double) -- ^ a
    (Length Double) -- ^ b
    (Length Double) -- ^ c
    (Angle Double) -- ^ alpha
    (Angle Double) -- ^ beta
    (Angle Double) -- ^ gamma
  deriving (Show)

-- | Sample

data Sample
  = Sample
    String -- ^ name of the sample
    Lattice -- ^ the lattice of the sample
    Parameter -- ^ ux
    Parameter -- ^ uy
    Parameter -- ^ uz
  deriving (Show)

-- | Source

data Source
  = Source
    (Length Double) -- ^ wavelength
  deriving (Show)

-- | Trajectory

type Trajectory = [[Double]]

-- | Hdf5

type H5Path = String
data ExtendDims = ExtendDims | StrictDims deriving (Show)
data DataItem = DataItem H5Path ExtendDims deriving (Show)
