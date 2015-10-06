module Hkl.Diffractometer
       ( e4c
       , Mode (..)
       , computeAngles
       , computeHkl
       , fromMode
       ) where
{-
    Copyright  : Copyright (C) 2014-2015 Synchrotron Soleil
    License    : GPL3+

    Maintainer : picca@synchrotron-soleil.fr
    Stability  : Experimental
    Portability: GHC only?
-}

import Data.Tree
import Prelude hiding (sqrt, sin, cos, (+), (-), (*), (**), (/))
import qualified Prelude

import Numeric.LinearAlgebra (Vector, Matrix,
                              fromList,
                              toList)

import Numeric.GSL.Root (root, RootMethod (Hybrids))

-- import Text.Printf (printf)

import Numeric.Units.Dimensional.Prelude (degree,
                                          Angle,
                                          (*~~), (/~~))

import Hkl.Lattice
import Hkl.Source
import Hkl.Transformation

-- diffractometer
-- data Diffractometer = Diffractometer [Angle Double -> Transformation] [Angle Double -> Transformation]
newtype Diffractometer = Tree (Angle Double -> Transformation)
data Mode = ModeHklE4CConstantPhi

e4c :: Tree (Angle Double -> Transformation)
e4c = Node base
      [ Node omega
        [ Node chi
          [ Node phi
            [ Node rx
              [ Node ry
                [ Node rz [] ]]]]]
      , Node tth []
      ]
  where
    base = NoTransformation
    rx = Rotation [1, 0, 0]
    ry = Rotation [0, 1, 0]
    rz = Rotation [0, 0, 1]
    omega = Rotation [0, -1, 0]
    chi = Rotation [1, 0, 0]
    phi = Rotation [0, -1, 0]
    tth = Rotation [0, -1, 0]

getSample :: Diffractometer -> [Angle Double -> Transformation]
getSample (Tree _ [x:xs]) = flatten x

getDetector :: Diffractometer -> [Angle Double -> Transformation]
getDetector (Tree _ [x:xs]) = flatten xs

computeHkl :: Diffractometer -> [Angle Double] -> Lattice -> Vector Double
computeHkl diffractometer values lattice =
    unapply q (Holder (zipWith ($) sample s ++ [UB lattice]))
        where
          sample = getSample diffractometer
          detector = getDetector diffractometer
          (s, d) = splitAt (length sample) values
          kf = apply (Holder (zipWith ($) detector d)) ki
          q = kf Prelude.- ki

fromMode :: Mode -> [Double] -> [Angle Double] -> [Angle Double]
fromMode ModeHklE4CConstantPhi fitted angles =
    newAngles
        where
          (_vs, _d) = splitAt 2 fitted
          (_cs, _) = splitAt 6 angles
          (_, _ccs) = splitAt 2 _cs
          newAngles = _vs *~~ degree ++ _ccs ++ _d *~~ degree

toMode :: Mode -> [Angle Double] -> [Double]
toMode ModeHklE4CConstantPhi angles =
    v
        where
          (_s, _d) = splitAt 6 angles
          (_ss, _) = splitAt 2 _s
          v = (_ss ++ _d) /~~ degree

computeAngles' :: Diffractometer -> [Angle Double] -> Lattice -> Mode -> [Double] -> [Double] -> [Double]
computeAngles' diffractometer angles lattice mode hkl fitted =
    toList (computeHkl diffractometer newAngles lattice Prelude.- fromList hkl)
        where
          newAngles = fromMode mode fitted angles

computeAngles :: Diffractometer -> [Angle Double] -> Lattice -> Mode -> [Double] -> ([Double], Matrix Double)
computeAngles diffractometer angles lattice mode hkl =
    root Hybrids 1E-7 30 f guess
         where
           f = computeAngles' diffractometer angles lattice mode hkl
           guess = toMode mode angles

