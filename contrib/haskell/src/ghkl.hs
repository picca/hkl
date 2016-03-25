module Main where

import Control.Monad
import Data.Map.Strict (lookup)
import Data.Maybe (isNothing, fromJust)
import Hkl
import Numeric.LinearAlgebra (fromList)
import Numeric.Units.Dimensional.Prelude (nano, meter, degree,
                                          (*~),
                                          (*~~), (/~~))
import Pipes
import qualified Pipes.Prelude as P
import Prelude hiding (lookup)

main' :: IO ()
main' = do
  factories <- factories
  let mfactory = lookup "E6C" factories
  if isNothing mfactory
  then
      return $ error $ "wrong diffractometer:" ++ show factories
  else do
    let factory = fromJust mfactory
    let sample = Sample "test" (Cubic (1.54 *~ nano meter))
                   (Parameter "ux" 0.0 (Range (-180) 180))
                   (Parameter "uy" 0.0 (Range (-180) 180))
                   (Parameter "uz" 0.0 (Range (-180) 180))

    let geometry = Geometry (Source (1.54 *~ nano meter)) [0, 30, 0, 0, 0, 60]
    let detector = Detector DetectorType0D

    -- compute the pseudo axes values
    pseudoAxes <- compute factory geometry detector sample
    -- print pseudoAxes

    -- solve a pseudo axis problem for the given engine
    let engine = Engine "hkl" [ Parameter "h" 0.0 (Range (-1.0) 1.0)
                              , Parameter "k" 0.0 (Range (-1.0) 1.0)
                              , Parameter "l" 1.0 (Range (-1.0) 1.0)
                              ]
                 (Mode "bissector_vertical" [])

    -- print =<< solve factory geometry detector sample engine
    let from = fromList [0, 0, 1 :: Double]
    let to = fromList [0, 1, 1 :: Double]
    runEffect $ fromToPipe 20 from to
              >-> P.print
    -- solve a trajectory with Pipes
    runEffect $ fromToPipe 1000 from to
              >-> enginesTrajectoryPipe engine
              >-> solveTrajPipe factory geometry detector sample
              -- >-> P.print
              >-> P.drain

    return ()

main :: IO ()
main = replicateM_ 1 main'
