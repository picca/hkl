module Hkl.MyMatrix
       ( Basis(..)
       , MyMatrix(..)
       , changeBase
       ) where

import Numeric.LinearAlgebra

data Basis = PyFAIB | HklB deriving (Show)

data MyMatrix a = MyMatrix Basis (Matrix a) deriving (Show)

changeBase :: MyMatrix Double -> Basis -> MyMatrix Double
changeBase (MyMatrix PyFAIB m) HklB = MyMatrix HklB (passage m p2)
changeBase (MyMatrix HklB m) PyFAIB = MyMatrix PyFAIB (passage m p1)
changeBase m@(MyMatrix PyFAIB _) PyFAIB = m
changeBase m@(MyMatrix HklB _) HklB = m

passage :: Matrix Double -> Matrix Double -> Matrix Double
passage r p = inv p <> r <> p

p1 :: Matrix Double -- hkl -> pyFAI
p1 = fromLists [ [0,  0, -1]
               , [0, -1,  0]
               , [1,  0,  0]]

p2 :: Matrix Double -- pyFAI -> hkl:
p2 = fromLists [ [ 0,  0, 1]
               , [ 0, -1, 0]
               , [-1,  0, 0]]
