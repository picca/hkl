module Hkl.Types.PyFAI
       ( PoniEntry(..)
       , Poni
       ) where

import Data.Text
import Numeric.Units.Dimensional.Prelude

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
