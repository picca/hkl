module Hkl.Types.PyFAI
       ( Poni(..)
       ) where

import Data.Text
-- import Numeric.Units.Dimensional.Prelude (Length, Angle)

-- | Poni

data Poni = Poni
            [Text] -- ^ header
            Text -- ^ Detector
            Double -- ^ pixels size 1
            Double -- ^ pixels size 2
            Double -- ^ distance
            Double -- ^ poni1
            Double -- ^ poni2
            Double -- ^ rot1
            Double -- ^ rot2
            Double -- ^ rot3
            Double -- ^ wavelength
            deriving (Show)
