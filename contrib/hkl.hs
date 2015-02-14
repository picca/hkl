{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fglasgow-exts -fallow-undecidable-instances #-}

import Prelude hiding (sqrt, sin, cos, (+), (-), (*), (**), (/))
import qualified Prelude

import Numeric.LinearAlgebra (fromLists, Vector, Matrix,
                              ident, scalar, fromList, outer,
                              (@>), (<>), vecdisp, disps, inv)
-- import Text.Printf (printf)

import Numeric.Units.Dimensional.Prelude (_0, _1, _2, nano, meter, degree,
                                          (*~), (/~), (+), (-), (*), (**), (/),
                                          Length, Angle, sin, cos, one, sqrt,
                                          Dimensionless)

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

-- A Transformation which can be apply to a Vector of Double
data Transformation = NoTransformation -- Doesn't transform the vector at all
                    | Rotation [Double] (Angle Double)
                    | UB Lattice
                    | Holder [Transformation]

tau :: Dimensionless Double
tau = _1 -- 1 or 2*pi

crossprod :: Vector Double -> Matrix Double
crossprod axis = fromLists [[0, -axis @> 2, axis @> 1],
                            [axis @> 2, 0, -axis @> 0],
                            [-axis @> 1, axis @> 0, 0]]

-- apply a transformation
apply :: Transformation -> Vector Double -> Vector Double
apply NoTransformation v = v
apply (Rotation axis angle) v = (scalar c Prelude.* ident 3
                                 Prelude.+ scalar s Prelude.* crossprod ax
                                 Prelude.+ scalar (1 Prelude.- c) Prelude.* ax `outer` ax) <> v
    where
      ax = fromList axis
      c = cos angle /~ one
      s = sin angle /~ one
apply (UB lattice) v = busing lattice <> v
apply (Holder t) v = foldr apply v t

-- unapply a transformation
unapply :: Vector Double -> Transformation -> Vector Double
unapply v NoTransformation = v
unapply v (Rotation axis angle) = apply (Rotation axis (_0 - angle)) v
unapply v (UB lattice) =  inv (busing lattice) <> v
unapply v (Holder t) = foldl unapply v t

-- source
lambda :: Length Double
lambda = 1.54 *~ nano meter

ki :: Vector Double
ki = fromList [(tau / lambda) /~ (one / meter), 0, 0]

-- diffractometer
data Diffractometer = Diffractometer [Angle Double -> Transformation] [Angle Double -> Transformation]

e4c :: Diffractometer
e4c = Diffractometer [omega, chi, phi, rx, ry, rz] [tth]
      where
        rx = Rotation [1, 0, 0]
        ry = Rotation [0, 1, 0]
        rz = Rotation [0, 0, 1]
        omega = Rotation [0, -1, 0]
        chi = Rotation [1, 0, 0]
        phi = Rotation [0, -1, 0]
        tth = Rotation [0, -1, 0]

computeHkl :: Diffractometer -> [Angle Double] -> [Angle Double] -> Lattice -> Vector Double
computeHkl (Diffractometer sample detector) s d lattice =
    unapply q (Holder (zipWith ($) sample s ++ [UB lattice]))
        where
          kf = apply (Holder (zipWith ($) detector d)) ki
          q = kf Prelude.- ki

dispv :: Vector Double -> IO ()
dispv = putStr . vecdisp (disps 2)

main :: IO()
main =
  dispv (computeHkl e4c s d lattice)
       where
         s = [30.0 *~ degree, 0.0 *~ degree, 0.0 *~ degree,
              00.0 *~ degree, 0.0 *~ degree, 0.0 *~ degree]
         d = [60.0 *~ degree]
         lattice = Cubic (1.54 *~ nano meter)

-- rosenbrock a b [x,y] = [ a*(1-x), b*(y-x^2) ]

-- disp = putStrLn . format "  " (printf "%.3f")

-- main = do
--    let (sol,path) = root Hybrids 1E-7 30 (rosenbrock 1 10) [-100,-5]
--    print sol
--    disp path
