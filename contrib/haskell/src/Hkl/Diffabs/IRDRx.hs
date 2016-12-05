{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Hkl.Diffabs.IRDRx
       ( mainIRDRx ) where

import Control.Concurrent.Async (mapConcurrently)
import Data.Array.Repa (DIM1, ix1)
import Data.Char (toUpper)
import Numeric.LinearAlgebra (ident)
import System.FilePath ((</>))
import Text.Printf (printf)

import Prelude hiding (concat, lookup, readFile, writeFile)

import Hkl.MyMatrix
import Hkl.PyFAI.PoniExt
import Hkl.Types
import Hkl.XRD
import Hkl.XRD.Calibration
import Hkl.Detector

-- | Samples

-- project = "/home/experiences/instrumentation/picca/data/99160066"
project :: FilePath
project = "/nfs/ruche-diffabs/diffabs-soleil/com-diffabs/"

published :: FilePath
published = project </> "2016" </> "Run5B" </> "irdrx"

beamlineUpper :: Beamline -> String
beamlineUpper b = [Data.Char.toUpper x | x <- show b]

nxs :: FilePath -> NxEntry -> (NxEntry -> DataFrameH5Path) -> Nxs
nxs f e h = Nxs f e (h e)

sampleRef :: XRDRef
sampleRef = XRDRef "reference"
            (published </> "calibration")
            (nxs (project </> "2016" </> "Run5" </> "2016-11-09" </> "scan_39.nxs") "scan_39" h5path')
            10

h5path' :: NxEntry -> DataFrameH5Path
h5path' nxentry =
    DataFrameH5Path { h5pImage = DataItem (nxentry </> image) StrictDims
                    , h5pGamma = DataItem (nxentry </> beamline </> gamma) ExtendDims
                    , h5pDelta = DataItem (nxentry </> delta) ExtendDims
                    , h5pWavelength = DataItem (nxentry </> beamline </> wavelength) StrictDims
                    }
        where
          beamline :: String
          beamline = beamlineUpper Diffabs

          image = "scan_data/data_05"
          gamma = "D13-1-CX1__EX__DIF.1-GAMMA__#1/raw_value"
          delta = "scan_data/data_03"
          wavelength = "D13-1-C03__OP__MONO__#1/wavelength"

sampleCalibration :: XRDCalibration
sampleCalibration = XRDCalibration { xrdCalibrationName = "calibration"
                                   , xrdCalibrationOutputDir = published </> "calibration" -- TODO pourquoi ce output
                                   , xrdCalibrationEntries = entries
                                   }
    where

      idxs :: [Int]
      idxs = [0, 1, 10, 30]

      entry :: Int -> XRDCalibrationEntry
      entry idx = XRDCalibrationEntry
                { xrdCalibrationEntryNxs = nxs (project </> "2016" </> "Run5" </> "2016-11-09" </> "scan_39.nxs") "scan_39" h5path'
                , xrdCalibrationEntryIdx = idx
                , xrdCalibrationEntryNptPath = published </> "calibration" </> printf "scan_39.nxs_%02d.npt" idx
                }

      entries :: [XRDCalibrationEntry]
      entries = [ entry idx | idx <- idxs]


bins :: DIM1
bins = ix1 1000

multibins :: DIM1
multibins = ix1 10000

threshold :: Threshold
threshold = Threshold 5000

lab6 :: XRDSample
lab6 = XRDSample "LaB6"
       (published </> "LaB6")
       [ XrdNxs bins multibins threshold n | n <-
         [ nxs (project </> "2016" </> "Run5" </> "2016-11-09" </> "scan_39.nxs") "scan_39" h5path'
         , nxs (project </> "2016" </> "Run5" </> "2016-11-09" </> "scan_40.nxs") "scan_40" h5path'
         , nxs (project </> "2016" </> "Run5" </> "2016-11-09" </> "scan_41.nxs") "scan_41" h5path'
         , nxs (project </> "2016" </> "Run5" </> "2016-11-09" </> "scan_42.nxs") "scan_42" h5path'
         , nxs (project </> "2016" </> "Run5" </> "2016-11-09" </> "scan_43.nxs") "scan_43" h5path'
         , nxs (project </> "2016" </> "Run5" </> "2016-11-09" </> "scan_44.nxs") "scan_44" h5path'
         , nxs (project </> "2016" </> "Run5" </> "2016-11-09" </> "scan_45.nxs") "scan_45" h5path'
         ]
       ]

-- | Main

mainIRDRx :: IO ()
mainIRDRx = do
  let samples = [lab6]

  p <- getPoniExtRef sampleRef

  let poniextref = setPose (Hkl.PyFAI.PoniExt.flip p) (MyMatrix HklB (ident 3))

  -- full calibration
  poniextref' <- calibrate sampleCalibration poniextref ImXpadS140

  -- integrate each step of the scan
  _ <- mapConcurrently (integrate poniextref') samples
  return ()
