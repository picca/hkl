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
    return ()

main :: IO ()
main = replicateM_ 1 main'
