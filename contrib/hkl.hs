{-# LANGUAGE ScopedTypeVariables #-}

import Numeric.LinearAlgebra (fromLists, Vector, Matrix,
                              ident, scalar, fromList, outer,
                              (@>), (<>), format, vecdisp, disps)
import Text.Printf (printf)

data Lattice = Cubic Double -- a = b = c, alpha = beta = gamma = 90
             | Tetragonal Double Double -- a = b != c, alpha = beta = gamma = 90
             | Orthorhombic Double Double Double -- a != b != c,  alpha = beta = gamma = 90
             | Rhombohedral Double Double -- a = b = c, alpha = beta = gamma != 90
             | Hexagonal Double Double -- a = b != c, alpha = beta = 90, gamma = 120
             | Monoclinic Double Double Double Double -- a != b != c, alpha = gamma = 90, beta != 90
             | Triclinic Double Double Double Double Double Double -- a != b != c, alpha != beta != gamma != 90
               deriving (Show)

data Transformation = Rotation [Double]
                      deriving (Show)

tau :: Double
tau = 1

radian :: Double -> Double
radian angle = angle * pi / 180.0

crossprod :: Vector Double -> Matrix Double
crossprod axis = fromLists [[0, -axis @> 2, axis @> 1],
                            [axis @> 2, 0, -axis @> 0],
                            [-axis @> 1, axis @> 0, 0]]

rotation :: [Double] -> Double -> Matrix Double
rotation axis angle = scalar c * ident 3
                      + scalar s * crossprod ax
                      + scalar (1 - c) * ax `outer` ax
  where
    ax = fromList axis
    c = cos(radian angle)
    s = sin(radian angle)

rx = Rotation [1, 0, 0]
ry = Rotation [0, 1, 0]
rz = Rotation [0, 0, 1]

omega = Rotation [0, -1, 0]
chi = Rotation [1, 0, 0]
phi = Rotation [0, -1, 0]

apply :: Transformation -> Double -> Matrix Double
apply t v = case t of
            Rotation axis -> rotation axis v

holder :: [Transformation] -> [Double] -> Matrix Double
holder t positions = foldl1 (<>) (zipWith apply t positions)

e4c :: [Double] -> Matrix Double
e4c = holder [omega, chi, phi]

u :: [Double] -> Matrix Double
u = holder [rx, ry, rz]

busing' :: Double -> Double -> Double -> Double -> Double -> Double -> Matrix Double
busing' a b c alpha beta gamma = fromLists [[b00, b01, b02],
                                            [0, b11, b12],
                                            [0, 0, b22]]
    where
      b00 = tau * sin alpha / (a * d)
      b01 = b11 / d * (cos alpha * cos beta - cos gamma)
      b02 = tmp / d * (cos gamma * cos alpha - cos beta)
      b11 = tau / (b * sin alpha)
      b12 = tmp / (sin beta * sin gamma) * (cos beta * cos gamma - cos alpha)
      b22 = tau / c
      d = sqrt(1 - cos alpha ** 2 - cos beta ** 2 - cos gamma ** 2 + 2 * cos alpha * cos beta * cos gamma)
      tmp = b22 / sin alpha;

busing :: Lattice -> Matrix Double
busing lattice = case lattice of
                   Cubic a                          -> busing' a a a 90 90 90
                   Tetragonal a c                   -> busing' a a c 90 90 90
                   Orthorhombic a b c               -> busing' a b c 90 90 90
                   Rhombohedral a alpha             -> busing' a a a alpha alpha alpha
                   Hexagonal a c                    -> busing' a a c 90 90 120
                   Monoclinic a b c beta            -> busing' a b c 90 beta 90
                   Triclinic a b c alpha beta gamma -> busing' a b c alpha beta gamma

ub :: [Double] -> Lattice -> Matrix Double
ub uxuyuz lattice = u uxuyuz <> busing lattice

sample :: [Double] -> [Double] -> Lattice -> [Double] -> Vector Double
sample motors uxuyuz lattice hkl = e4c motors <> ub uxuyuz lattice <> fromList hkl

disp :: Matrix Double -> IO ()
disp = putStrLn . format "  " (printf "%.3f")

dispv :: Vector Double -> IO ()
dispv = putStr . vecdisp (disps 2)

main :: IO()
main =  dispv (sample [30, 30, 30] [0, 90, 0] (Cubic 1.54) [1, 1, 1])

-- rosenbrock a b [x,y] = [ a*(1-x), b*(y-x^2) ]

-- disp = putStrLn . format "  " (printf "%.3f")

-- main = do
--    let (sol,path) = root Hybrids 1E-7 30 (rosenbrock 1 10) [-100,-5]
--    print sol
--    disp path
