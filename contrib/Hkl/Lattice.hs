{-# OPTIONS_GHC -Werror #-}

module Hkl.Lattice
       ( tau
       , Lattice (..)
       , busing
       ) where
{-
    Copyright  : Copyright (C) 2014-2015 Synchrotron Soleil
    License    : GPL3+

    Maintainer : picca@synchrotron-soleil.fr
    Stability  : Experimental
    Portability: GHC only?
-}

import Prelude hiding (sqrt, sin, cos, (+), (-), (*), (**), (/))
import qualified Prelude

import Numeric.LinearAlgebra (fromLists, Matrix)

import Numeric.Units.Dimensional.Prelude (_1, _2, meter, degree,
                                          (*~), (/~), (+), (-), (*), (**), (/),
                                          Length, Angle, sin, cos, one, sqrt,
                                          Dimensionless)


tau :: Dimensionless Double
tau = _1 -- 1 or 2*pi

data Lattice = Cubic (Length Double) -- a = b = c, alpha = beta = gamma = 90
             | Tetragonal (Length Double) (Length Double) -- a = b != c, alpha = beta = gamma = 90
             | Orthorhombic (Length Double) (Length Double) (Length Double) -- a != b != c,  alpha = beta = gamma = 90
             | Rhombohedral (Length Double) (Angle Double) -- a = b = c, alpha = beta = gamma != 90
             | Hexagonal (Length Double) (Length Double) -- a = b != c, alpha = beta = 90, gamma = 120
             | Monoclinic (Length Double) (Length Double) (Length Double) (Angle Double) -- a != b != c, alpha = gamma = 90, beta != 90
             | Triclinic (Length Double) (Length Double) (Length Double) (Angle Double) (Angle Double) (Angle Double) -- a != b != c, alpha != beta != gamma != 90
               deriving (Show)

busing' :: Length Double -> Length Double -> Length Double -> Angle Double -> Angle Double-> Angle Double -> Matrix Double
busing' a b c alpha beta gamma = fromLists [[b00 /~ (one / meter), b01/~ (one / meter), b02/~ (one / meter)],
                                            [0  , b11 /~ (one / meter), b12 /~ (one / meter)],
                                            [0  , 0  , b22 /~ (one / meter)]]
    where
      b00 = tau * sin alpha / (a * d)
      b01 = b11 / d * (cos alpha * cos beta - cos gamma)
      b02 = tmp / d * (cos gamma * cos alpha - cos beta)
      b11 = tau / (b * sin alpha)
      b12 = tmp / (sin beta * sin gamma) * (cos beta * cos gamma - cos alpha)
      b22 = tau / c
      d = sqrt(_1 - cos alpha ** _2 - cos beta ** _2 - cos gamma ** _2 + _2 * cos alpha * cos beta * cos gamma)
      tmp = b22 / sin alpha;

busing :: Lattice -> Matrix Double
busing (Cubic a) = busing' a a a (90 *~ degree) (90 *~ degree) (90 *~ degree)
busing (Tetragonal a c) = busing' a a c (90 *~ degree) (90 *~ degree) (90 *~ degree)
busing (Orthorhombic a b c) = busing' a b c (90 *~ degree) (90 *~ degree) (90 *~ degree)
busing (Rhombohedral a alpha)= busing' a a a alpha alpha alpha
busing (Hexagonal a c) = busing' a a c (90 *~ degree) (90 *~ degree) (120 *~ degree)
busing (Monoclinic a b c beta) = busing' a b c (90 *~ degree) beta (90 *~ degree)
busing (Triclinic a b c alpha beta gamma) = busing' a b c alpha beta gamma
