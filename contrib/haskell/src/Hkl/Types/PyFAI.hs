module Hkl.Types.PyFAI
       ( PoniEntry(..)
       , Poni
       ) where

import Data.Text
-- import Numeric.Units.Dimensional.Prelude (Length, Angle)

-- | Poni

data PoniEntry = PoniEntry
            [Text] -- ^ header
            (Maybe Text) -- ^ Detector
            Double -- ^ pixels size 1
            Double -- ^ pixels size 2
            Double -- ^ distance
            Double -- ^ poni1
            Double -- ^ poni2
            Double -- ^ rot1
            Double -- ^ rot2
            Double -- ^ rot3
            (Maybe Text) -- ^ spline file
            Double -- ^ wavelength
            deriving (Show)

type Poni = [PoniEntry]
