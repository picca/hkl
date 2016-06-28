module Hkl.Types.PyFAI
       ( Poni(..)
       ) where

import Numeric.Units.Dimensional.Prelude (Length, Angle)

-- | Poni

data Poni = Poni
            (Length Double) -- ^ distance
            (Length Double) -- ^ poni1
            (Length Double) -- ^ poni2
            (Angle Double) -- ^ rot1
            (Angle Double) -- ^ rot2
            (Angle Double) -- ^ rot3
