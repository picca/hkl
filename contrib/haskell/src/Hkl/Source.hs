{-
    Copyright  : Copyright (C) 2014-2016 Synchrotron Soleil
    License    : GPL3+

    Maintainer : picca@synchrotron-soleil.fr
    Stability  : Experimental
    Portability: GHC only?
-}
module Hkl.Source
       ( ki ) where

import Prelude hiding ((/))

import Numeric.LinearAlgebra (Vector, fromList)

import Numeric.Units.Dimensional.Prelude (nano, meter,
                                          (*~), (/~), (/), Length, one)
import Hkl.Lattice (tau)

lambda :: Length Double
lambda = 1.54 *~ nano meter

ki :: Vector Double
ki = fromList [(tau / lambda) /~ (one / meter), 0, 0]
