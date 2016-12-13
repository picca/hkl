{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Hkl.D2AM.XRD
       ( d2am ) where

-- import Control.Concurrent.Async (mapConcurrently)
-- import Data.Array.Repa (DIM1, ix1)
-- import Data.Char (toUpper)
import Numeric.LinearAlgebra (ident)
import System.FilePath ((</>))
import Text.Printf (printf)

import Prelude hiding (concat, lookup, readFile, writeFile)

import Hkl.MyMatrix
import Hkl.PyFAI.PoniExt
-- import Hkl.Types
import Hkl.XRD
import Hkl.XRD.Calibration
import Hkl.Detector

-- | Samples

project :: FilePath
project = "/home/experiences/instrumentation/picca/data/d2am"
-- project = "/nfs/ruche-diffabs/diffabs-soleil/com-diffabs/"

published :: FilePath
published = project </> "published-data"

sampleRef :: XRDRef
sampleRef =  XRDRefEdf "reference"
            (published </> "calibration")
            (project </> "16Dec08D5_0268-rsz.edf")
            (project </> "16Dec08D5_0268-rsz.poni")

sampleCalibration :: XRDCalibration
sampleCalibration = XRDCalibration { xrdCalibrationName = "calibration"
                                   , xrdCalibrationOutputDir = published </> "calibration" -- TODO pourquoi ce output
                                   , xrdCalibrationEntries = entries
                                   }
    where

      idxs :: [Int]
      idxs = [268, 271, 285, 295]

      entry :: Int -> XRDCalibrationEntry
      entry idx = XRDCalibrationEntryEdf
                { xrdCalibrationEntryEdf'Edf = project </> printf "16Dec08D5_%04d-rsz.edf" idx
                , xrdCalibrationEntryEdf'NptPath = project </> printf "16Dec08D5_%04d-rsz.npt" idx
                }

      entries :: [XRDCalibrationEntry]
      entries = [ entry idx | idx <- idxs]

-- | Main

d2am :: IO ()
d2am = do
  -- let samples = [lab6]

  p <- getPoniExtRef sampleRef

  -- let poniextref = setPose (Hkl.PyFAI.PoniExt.flip p) (MyMatrix HklB (ident 3))
  let poniextref = setPose p (MyMatrix HklB (ident 3))

  -- full calibration
  poniextref' <- calibrate sampleCalibration poniextref Xpad32

  print poniextref
  print poniextref'

  -- integrate each step of the scan
  -- _ <- mapConcurrently (integrate poniextref') samples
  return ()
