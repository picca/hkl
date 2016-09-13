{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Hkl.Diffabs.Martinetto
       ( main_martinetto
       , main_calibration'
       ) where

import Control.Concurrent.Async
import Data.Char (toUpper)
import Numeric.Units.Dimensional.Prelude (meter, nano, (*~))
import System.FilePath ((</>))
import Prelude hiding (concat, lookup, readFile, writeFile)
import Text.Printf (printf)

import Hkl.PyFAI
import Hkl.Types
import Hkl.XRD
import Hkl.Detector

sampleCalibration :: XRDCalibration
sampleCalibration = XRDCalibration { xrdCalibrationName = "calibration"
                                   , xrdCalibrationOutputDir = published </> "calibration"
                                   , xrdCalibrationEntries = entries
                                   }
    where
      beamline :: String
      beamline = beamlineUpper Diffabs

      image = "scan_data/data_53"
      gamma = "d13-1-cx1__EX__DIF.1-GAMMA__#1/raw_value"
      delta = "scan_data/actuator_1_1"
      wavelength = "D13-1-C03__OP__MONO__#1/wavelength"

      idxs = [3 :: Int, 6, 9, 15, 18, 21, 24, 27, 30, 33, 36, 39, 43]

      h5path' :: NxEntry -> DataFrameH5Path
      h5path' nxentry =
          DataFrameH5Path { h5pImage = DataItem (nxentry </> image) StrictDims
                          , h5pGamma = DataItem (nxentry </> beamline </> gamma) ExtendDims
                          , h5pDelta = DataItem (nxentry </> delta) ExtendDims
                          , h5pWavelength = DataItem (nxentry </> beamline </> wavelength) StrictDims
                          }

      entry i = XRDCalibrationEntry
                { xrdCalibrationEntryNxs = nxs (published </> "calibration" </> "XRD18keV_26.nxs") "scan_26" h5path'
                , xrdCalibrationEntryIdx = i
                , xrdCalibrationEntryNptPath = published </> "calibration" </> printf "XRD18keV_26.nxs_%02d.npt" i
                }

      entries = [ entry i | i <- idxs]

-- | Samples

-- project = "/home/experiences/instrumentation/picca/data/99160066"
project :: FilePath
project = "/nfs/ruche-diffabs/diffabs-users/99160066/"
-- project = "/home/picca/data/99160066"

published :: FilePath
published = project </> "published-data"

beamlineUpper :: Beamline -> String
beamlineUpper b = [Data.Char.toUpper x | x <- show b]

nxs :: FilePath -> NxEntry -> (NxEntry -> DataFrameH5Path) -> Nxs
nxs f e h = Nxs f e (h e)

sampleRef :: XRDRef
sampleRef = XRDRef "reference"
            (published </> "calibration")
            (nxs (published </> "calibration" </> "XRD18keV_26.nxs") "scan_26" h5path')
            18
  where
    beamline :: String
    beamline = beamlineUpper Diffabs

    image = "scan_data/data_53"
    gamma = "d13-1-cx1__EX__DIF.1-GAMMA__#1/raw_value"
    delta = "scan_data/actuator_1_1"
    wavelength = "D13-1-C03__OP__MONO__#1/wavelength"

    h5path' :: NxEntry -> DataFrameH5Path
    h5path' nxentry =
      DataFrameH5Path { h5pImage = DataItem (nxentry </> image) StrictDims
                      , h5pGamma = DataItem (nxentry </> beamline </> gamma) ExtendDims
                      , h5pDelta = DataItem (nxentry </> delta) ExtendDims
                      , h5pWavelength = DataItem (nxentry </> beamline </> wavelength) StrictDims
                      }


h5path :: NxEntry -> DataFrameH5Path
h5path nxentry =
  DataFrameH5Path { h5pImage = DataItem (nxentry </> image) StrictDims
                  , h5pGamma = DataItem (nxentry </> beamline </> gamma) ExtendDims
                  , h5pDelta = DataItem (nxentry </> delta) ExtendDims
                  , h5pWavelength = DataItem (nxentry </> beamline </> wavelength) StrictDims
                  }
  where
    beamline :: String
    beamline = beamlineUpper Diffabs

    image = "scan_data/data_58"
    gamma = "D13-1-CX1__EX__DIF.1-GAMMA__#1/raw_value"
    delta = "scan_data/actuator_1_1"
    wavelength = "D13-1-C03__OP__MONO__#1/wavelength"

bins :: Bins
bins = Bins 8000

threshold :: Threshold
threshold = Threshold 800

n27t2 :: XRDSample
n27t2 = XRDSample "N27T2"
        (published </> "N27T2")
        [ XrdNxs bins threshold n | n <-
          [ nxs (project </> "2016" </> "Run2" </> "2016-03-27" </> "N27T2_14.nxs") "scan_14" h5path
          , nxs (project </> "2016" </> "Run2" </> "2016-03-27" </> "N27T2_17.nxs") "scan_17" h5path
          ]
        ]

r34n1 :: XRDSample
r34n1 = XRDSample "R34N1"
        (published </> "R34N1")
        [ XrdNxs bins threshold n | n <-
          [ nxs (project </> "2016" </> "Run2" </> "2016-03-27" </> "R34N1_28.nxs") "scan_28" h5path
          , nxs (project </> "2016" </> "Run2" </> "2016-03-27" </> "R34N1_37.nxs") "scan_37" h5path
          ]
        ]

r23 :: XRDSample
r23 = XRDSample "R23"
        (published </> "R23")
        [ XrdNxs bins threshold n | n <-
          [ nxs (project </> "2016" </> "Run2" </> "2016-03-27" </> "R23_6.nxs") "scan_6" h5path
          , nxs (project </> "2016" </> "Run2" </> "2016-03-27" </> "R23_12.nxs") "scan_12" h5path
          ]
        ]

r18 :: XRDSample
r18 = XRDSample "R18"
        (published </> "R18")
        [ XrdNxs bins threshold n | n <-
          [ nxs (project </> "2016" </> "Run2" </> "2016-03-27" </> "R18_20.nxs") "scan_20" h5path
          , nxs (project </> "2016" </> "Run2" </> "2016-03-27" </> "R18_24.nxs") "scan_24" h5path
          ]
        ]

a3 :: XRDSample
a3 = XRDSample "A3"
        (published </> "A3")
        [ XrdNxs bins threshold n | n <-
          [ nxs (project </> "2016" </> "Run2" </> "2016-03-27" </> "A3_13.nxs") "scan_13" h5path
          , nxs (project </> "2016" </> "Run2" </> "2016-03-27" </> "A3_14.nxs") "scan_14" h5path
          , nxs (project </> "2016" </> "Run2" </> "2016-03-27" </> "A3_15.nxs") "scan_15" h5path
          ]
        ]

a2 :: XRDSample
a2 = XRDSample "A2"
        (published </> "A2")
        [ XrdNxs bins threshold n | n <-
         [ nxs (project </> "2016" </> "Run2" </> "2016-03-27" </> "A2_14.nxs") "scan_14" h5path
         , nxs (project </> "2016" </> "Run2" </> "2016-03-27" </> "A2_17.nxs") "scan_17" h5path
         ]
        ]

d2 :: XRDSample
d2 = XRDSample "D2"
        (published </> "D2")
        [ XrdNxs bins threshold n | n <-
          [ nxs (project </> "2016" </> "Run2" </> "2016-03-27" </> "D2_16.nxs") "scan_16" h5path
          , nxs (project </> "2016" </> "Run2" </> "2016-03-27" </> "D2_17.nxs") "scan_17" h5path
          ]
        ]

d3 :: XRDSample
d3 = XRDSample "D3"
        (published </> "D3")
        [ XrdNxs bins threshold n | n <-
          [ nxs (project </> "2016" </> "Run2" </> "2016-03-27" </> "D3_14.nxs") "scan_14" h5path
          , nxs (project </> "2016" </> "Run2" </> "2016-03-27" </> "D3_15.nxs") "scan_15" h5path
          ]
        ]

r11 :: XRDSample
r11 = XRDSample "R11"
        (published </> "R11")
        [ XrdNxs bins threshold n | n <-
          [ nxs (project </> "2016" </> "Run2" </> "2016-03-27" </> "R11_5.nxs") "scan_5" h5path
          , nxs (project </> "2016" </> "Run2" </> "2016-03-27" </> "R11_6.nxs") "scan_6" h5path
          , nxs (project </> "2016" </> "Run2" </> "2016-03-27" </> "R11_7.nxs") "scan_7" h5path
          ]
        ]

d16 :: XRDSample
d16 = XRDSample "D16"
        (published </> "D16")
        [ XrdNxs bins threshold n | n <-
          [ nxs (project </> "2016" </> "Run2" </> "2016-03-27" </> "D16_12.nxs") "scan_12" h5path
          , nxs (project </> "2016" </> "Run2" </> "2016-03-27" </> "D16_15.nxs") "scan_15" h5path
          , nxs (project </> "2016" </> "Run2" </> "2016-03-27" </> "D16_17.nxs") "scan_17" h5path
          ]
        ]

k9a2 :: XRDSample
k9a2 = XRDSample "K9A2"
       (published </> "K9A2")
        [ XrdNxs bins threshold n | n <-
          [ nxs (project </> "2016" </> "Run2" </> "2016-03-27" </> "K9A2_1_31.nxs") "scan_31" h5path
          , nxs (project </> "2016" </> "Run2" </> "2016-03-27" </> "K9A2_1_32.nxs") "scan_32" h5path
          ]
        ]

-- | Main

main_martinetto :: IO ()
main_martinetto = do
  -- lire le ou les ponis de référence ainsi que leur géométrie
  -- associée.

  let samples = [n27t2, r34n1, r23, r18, a2, a3, d2, d3, r11, d16, k9a2]

  (PoniExt p m) <- getPoniExtRef sampleRef

  -- flip the ref poni in order to fit the reality
  let poniextref = PoniExt [flipPoniEntry e | e <- p] m

  -- integrate each step of the scan
  _ <- mapConcurrently (integrate poniextref) samples

  -- plot de la figure. (script python ou autre ?)
  return ()

main_calibration' :: IO ()
main_calibration' = do
  -- let samples = [n27t2, r34n1, r23, r18, a2, a3, d2, d3, r11, d16, k9a2]

  p <- getPoniExtRef sampleRef
  print p
  poniextref <- calibrate sampleCalibration (0.068877 *~ nano meter) DetectorXpad32
  print poniextref

  -- integrate each step of the scan
  -- _ <- mapConcurrently (integrate poniextref) samples
  return ()
