module Hkl.Transformation
       ( Transformation (..)
       , apply
       , unapply
       )where
{-
    Copyright  : Copyright (C) 2014-2016 Synchrotron Soleil
    License    : GPL3+

    Maintainer : picca@synchrotron-soleil.fr
    Stability  : Experimental
    Portability: GHC only?
-}

import Prelude hiding (sqrt, sin, cos, (+), (-), (*), (**), (/))
import qualified Prelude

import Numeric.LinearAlgebra (fromLists, Vector, Matrix,
                              ident, scalar, fromList,
                              (@>), (<>), inv)

import Numeric.Units.Dimensional.Prelude (_0, (-), (/~),
                                          Angle, sin, cos, one)
import Hkl.Lattice

-- A Transformation which can be apply to a Vector of Double
data Transformation = NoTransformation -- Doesn't transform the vector at all
                    | Rotation [Double] (Angle Double)
                    | UB Lattice
                    | Holder [Transformation]

crossprod :: Vector Double -> Matrix Double
crossprod axis = fromLists [[ 0, -z,  y],
                            [ z,  0, -x],
                            [-y,  x,  0]]
    where
      x = axis @> 0
      y = axis @> 1
      z = axis @> 2

-- apply a transformation
apply :: Transformation -> Vector Double -> Vector Double
apply NoTransformation v = v
apply (Rotation axis angle) v = (ident 3 Prelude.+ s Prelude.* q Prelude.+ c Prelude.* (q <> q)) <> v
    where
      ax = fromList axis
      c = scalar (1 Prelude.- cos angle /~ one)
      s = scalar (sin angle /~ one)
      q = crossprod ax
apply (UB lattice) v = busing lattice <> v
apply (Holder t) v = foldr apply v t

-- unapply a transformation
unapply :: Vector Double -> Transformation -> Vector Double
unapply v NoTransformation = v
unapply v (Rotation axis angle) = apply (Rotation axis (_0 - angle)) v
unapply v (UB lattice) =  inv (busing lattice) <> v
unapply v (Holder t) = foldl unapply v t
