module Main where

import Control.Monad
import Data.Vector.Storable (fromList)
import Numeric.Units.Dimensional.Prelude (nano, meter,
                                          (*~))
-- import Pipes
-- import qualified Pipes.Prelude as P

import Hkl

import Prelude hiding (lookup)

main :: IO ()
main = do
  let sample = Sample "test" (Orthorhombic
                              (1.05394 *~ nano meter)
                              (0.25560 *~ nano meter)
                              (1.49050 *~ nano meter))
               (Parameter "ux" (-89.8821) (Range (-180) 180))
               (Parameter "uy" 0.1733 (Range (-180) 180))
               (Parameter "uz" (-84.0081) (Range (-180) 180))

  let geometry = Geometry Uhv (Source (0.0672929 *~ nano meter))
                 (fromList [0.1794, -160.0013, 21.1381, 0.5194])
                 (Just [ Parameter "mu" 0.1794 (Range (-180) 180)
                       , Parameter "omega" (-160.0013) (Range (-180) 180)
                       , Parameter "delta" 21.1381 (Range (-180) 180)
                       , Parameter "gamma" 0.5194 (Range (-180) 180)])
  let detector = ZeroD

  -- compute the pseudo axes values
  pseudoAxes <- compute geometry detector sample
  print pseudoAxes

  -- solve a pseudo axis problem for the given engine
  let engine = Engine "hkl" [ Parameter "h" 4.0 (Range (-1.0) 1.0)
                            , Parameter "k" 1.0 (Range (-1.0) 1.0)
                            , Parameter "l" 0.3 (Range (-1.0) 1.0)
                            ]
               (Mode "zaxis" [])

  print =<< solve geometry detector sample engine

    -- let from = fromList [0, 0, 1 :: Double]
    -- let to = fromList [0, 1, 1 :: Double]
    -- runEffect $ fromToPipe 20 from to
    --           >-> P.print
    -- -- solve a trajectory with Pipes
    -- runEffect $ fromToPipe 10000 from to
    --           >-> enginesTrajectoryPipe engine
    --           >-> solveTrajPipe factory geometry detector sample
    --           >-> P.print
    --           -- >-> P.drain

  return ()
