module Hkl.MyMatrix
       ( Basis(..)
       , MyMatrix(..)
       , changeBase
       , toEulerians
       ) where

import Numeric.LinearAlgebra
import Numeric.Units.Dimensional.Prelude (Angle, (*~), radian)

data Basis = PyFAIB -- the pyFAI (1, 2, 3) detector coordinates
           | HklB -- the hkl coordinates
           deriving (Show)

data MyMatrix a = MyMatrix Basis (Matrix a) deriving (Show)

changeBase :: MyMatrix Double -> Basis -> MyMatrix Double
changeBase (MyMatrix PyFAIB m) HklB = MyMatrix HklB (passage m p2)
changeBase (MyMatrix HklB m) PyFAIB = MyMatrix PyFAIB (passage m p1)
changeBase m@(MyMatrix PyFAIB _) PyFAIB = m
changeBase m@(MyMatrix HklB _) HklB = m

passage :: Matrix Double -> Matrix Double -> Matrix Double
passage r p = inv p <> r <> p

p1 :: Matrix Double -- hkl -> pyFAI
p1 = fromLists [ [0,  0, 1]
               , [0, -1, 0]
               , [1,  0, 0]]

p2 :: Matrix Double -- pyFAI -> hkl:
p2 = fromLists [ [ 0,  0, 1]
               , [ 0, -1, 0]
               , [ 1,  0, 0]]

toEulerians :: Matrix Double -> (Angle Double, Angle Double, Angle Double)
toEulerians m
  | abs c > epsilon = ( atan2 ((m `atIndex` (2, 1)) / c) ((m `atIndex` (2, 2)) / c) *~ radian
                      , rot2 *~ radian
                      , atan2 ((m `atIndex` (1, 0)) / c) ((m `atIndex` (0, 0)) / c) *~ radian
                      )
  | otherwise        = ( 0 *~ radian
                       , rot2 *~ radian
                       , atan2 (-(m `atIndex` (0, 1))) (m `atIndex` (1, 1)) *~ radian
                       )
  where
    epsilon = 1e-10
    rot2 = asin (-(m `atIndex` (2, 0)))
    c = cos rot2

