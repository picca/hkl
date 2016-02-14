module Main where

import Control.Monad
import Data.Map.Strict (lookup)
import Data.Maybe (isNothing, fromJust)
import Hkl
import Numeric.Units.Dimensional.Prelude (nano, meter, degree,
                                          (*~),
                                          (*~~), (/~~))
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
    let geometry = Geometry (Source (1.54 *~ nano meter)) [0, 30, 0, 0, 0, 60]
    let detector = Detector DetectorType0D

    -- compute the pseudo axes values
    pseudoAxes <- compute factory geometry detector sample
    print pseudoAxes

    -- solve the pseudo axis problem
    let engine = Engine "hkl" [ Parameter "h" 0.0 (Range (-1.0) 1.0)
                              , Parameter "k" 0.0 (Range (-1.0) 1.0)
                              , Parameter "l" 1.0 (Range (-1.0) 1.0)
                              ]
                 (Mode "bissector_vertical" [])
    solutions <- solve factory geometry detector sample engine
    print solutions
    return ()

main :: IO ()
main = replicateM_ 1 main'
