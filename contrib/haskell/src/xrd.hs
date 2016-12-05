module Main where

import Control.Monad
import Hkl

main :: IO ()
main = do
  replicateM_ 1 mainIRDRx
  -- main = replicateM_ 1 main'
  -- replicateM_ 1 martinetto'
  -- main = replicateM_ 1 main_calibration'
