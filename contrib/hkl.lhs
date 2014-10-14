> {-# LANGUAGE ScopedTypeVariables #-}

> import Numeric.LinearAlgebra (fromLists, Vector, Matrix,
>                              ident, scalar, fromList, outer,
>                              (@>), (<>), format)
> import Text.Printf (printf)

> radian :: Double -> Double
> radian angle = angle * pi / 180.0

> crossprod :: Vector Double -> Matrix Double
> crossprod axis = fromLists [[0, -axis @> 2, axis @> 1],
>                             [axis @> 2, 0, -axis @> 0],
>                             [-axis @> 1, axis @> 0, 0]]
  
> rotation :: Double -> Vector Double -> Matrix Double
> rotation angle axis = scalar c * ident 3
>                       + scalar s * crossprod axis
>                       + scalar (1 - c) * axis `outer` axis
>   where
>     c = cos(radian angle)
>     s = sin(radian angle)

> omega :: Double -> Matrix Double
> omega angle = rotation angle (fromList [0, -1, 0])

> chi :: Double -> Matrix Double
> chi angle = rotation angle (fromList [1, 0, 0])

> phi :: Double -> Matrix Double
> phi angle = rotation angle (fromList [0, -1, 0])

> e4c :: [Double] -> Matrix Double
> e4c positions = foldl1 (<>) (zipWith ($) [omega, chi, phi] positions)

> busing :: Double -> Double -> Double -> Double -> Double -> Double -> Matrix Double
> busing a b c alpha beta gamma = fromLists [[b00, b01, b02],
>                                            [0, b11, b12],
>                                            [0, 0, b22]]
>   where
>     tau = 1
>     b00 = tau * sin alpha / (a * d)
>     b01 = b11 / d * (cos alpha * cos beta - cos gamma)
>     b02 = tmp / d * (cos gamma * cos alpha - cos beta)
>     b11 = tau / (b * sin alpha)
>     b12 = tmp / (sin beta * sin gamma) * (cos beta * cos gamma - cos alpha)
>     b22 = tau / c
>     d = sqrt(1 - cos alpha ** 2 - cos beta ** 2 - cos gamma ** 2 + 2 * cos alpha * cos beta * cos gamma)
>     tmp = b22 / sin alpha;

> disp :: Matrix Double -> IO ()
> disp = putStrLn . format "  " (printf "%.3f")

> main :: IO()
> main =  disp (e4c [90, 90, 90])

-- rosenbrock a b [x,y] = [ a*(1-x), b*(y-x^2) ]

-- disp = putStrLn . format "  " (printf "%.3f")

-- main = do
--    let (sol,path) = root Hybrids 1E-7 30 (rosenbrock 1 10) [-100,-5]
--    print sol
--    disp path
