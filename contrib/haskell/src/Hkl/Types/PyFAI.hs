module Hkl.Types.PyFAI
       ( Basis(..)
       , MyMatrix(..)
       , PoniEntry(..)
       , Poni
       ) where

import Numeric.LinearAlgebra
import Data.Text
import Numeric.Units.Dimensional.Prelude

data Basis = PyFAIB | HklB deriving (Show)

data MyMatrix a = MyMatrix Basis (Matrix a) deriving (Show)

-- | Poni

data PoniEntry = PoniEntry
            [Text] -- ^ header
            (Maybe Text) -- ^ Detector
            (Length Double) -- ^ pixels size 1
            (Length Double) -- ^ pixels size 2
            (Length Double) -- ^ distance
            (Length Double) -- ^ poni1
            (Length Double) -- ^ poni2
            (Angle Double) -- ^ rot1
            (Angle Double) -- ^ rot2
            (Angle Double) -- ^ rot3
            (Maybe Text) -- ^ spline file
            (Length Double) -- ^ wavelength
            deriving (Show)

type Poni = [PoniEntry]
