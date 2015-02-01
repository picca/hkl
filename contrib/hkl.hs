{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fglasgow-exts -fallow-undecidable-instances #-}

import Prelude hiding (sqrt, sin, cos, (+), (-), (*), (**), (/))
import qualified Prelude

import Numeric.LinearAlgebra (fromLists, Vector, Matrix,
                              ident, scalar, fromList, outer,
                              (@>), (<>), vecdisp, disps)
-- import Text.Printf (printf)

import Numeric.Units.Dimensional.Prelude (_1, _2, nano, meter, degree,
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

data Transformation = NoTransformation -- Doesn't transform the vector at all
                    | Rotation [Double] (Angle Double)
                    | UB Lattice

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

-- define a few transformations
rx, ry, rz, omega, chi, phi, tth :: Angle Double -> Transformation
rx = Rotation [1, 0, 0]
ry = Rotation [0, 1, 0]
rz = Rotation [0, 0, 1]
omega = Rotation [0, -1, 0]
chi = Rotation [1, 0, 0]
phi = Rotation [0, -1, 0]
tth = Rotation [0, -1, 0]

apply' :: [Transformation] -> Vector Double -> Vector Double
apply' t v = foldr apply v t

holder :: [Angle Double -> Transformation] -> [Angle Double] -> [Transformation]
holder t positions = zipWith ($) t positions

e4cS :: [Angle Double -> Transformation]
e4cS = [omega, chi, phi, rx, ry, rz]

e4cD :: [Angle Double -> Transformation]
e4cD = [tth]

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

sample :: [Angle Double] -> Lattice -> Vector Double -> Vector Double
sample positions lattice = apply' (holder e4cS positions ++ [UB lattice])

lambda :: Length Double
lambda = 1.54 *~ nano meter

ki :: Vector Double
ki = fromList [(tau / lambda) /~ (one / meter), 0, 0]

detector :: [Angle Double] -> Vector Double
detector positions = apply' (holder e4cD positions) ki Prelude.- ki

-- disp :: Matrix Double -> IO ()
-- disp = putStrLn . format "  " (printf "%.3f")

dispv :: Vector Double -> IO ()
dispv = putStr . vecdisp (disps 2)

main :: IO()
main =  do
  dispv (sample [30.0 *~ degree, 0.0 *~ degree, 0.0 *~ degree,
                 0.0 *~ degree, 0.0 *~ degree, 0.0 *~ degree] (Cubic (1.54 *~ nano meter))
         (fromList [0, 0, 1]))
  dispv (detector [60.0 *~ degree])

-- rosenbrock a b [x,y] = [ a*(1-x), b*(y-x^2) ]

-- disp = putStrLn . format "  " (printf "%.3f")

-- main = do
--    let (sol,path) = root Hybrids 1E-7 30 (rosenbrock 1 10) [-100,-5]
--    print sol
--    disp path
