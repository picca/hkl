{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Hkl.Diffabs.Martinetto
       ( martinetto
       , martinetto'
       ) where

import Control.Concurrent (setNumCapabilities)
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
project = "/nfs/ruche-diffabs/diffabs-users/99160066/"
-- project = "/home/picca/data/99160066"

published :: FilePath
published = project </> "published-data"

beamlineUpper :: Beamline -> String
beamlineUpper b = [Data.Char.toUpper x | x <- show b]

nxs :: FilePath -> NxEntry -> (NxEntry -> DataFrameH5Path) -> Nxs
nxs f e h = Nxs f e (h e)

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

          image = "scan_data/data_53"
          gamma = "d13-1-cx1__EX__DIF.1-GAMMA__#1/raw_value"
          delta = "scan_data/actuator_1_1"
          wavelength = "D13-1-C03__OP__MONO__#1/wavelength"

sampleCalibration :: XRDCalibration
sampleCalibration = XRDCalibration { xrdCalibrationName = "calibration"
                                   , xrdCalibrationOutputDir = published </> "calibration"
                                   , xrdCalibrationEntries = entries
                                   }
    where

      idxs :: [Int]
      idxs = [3, 6, 9, 15, 18, 21, 24, 27, 30, 33, 36, 39, 43]

      entry :: Int -> XRDCalibrationEntry
      entry idx = XRDCalibrationEntry
                { xrdCalibrationEntryNxs = nxs (published </> "calibration" </> "XRD18keV_26.nxs") "scan_26" h5path'
                , xrdCalibrationEntryIdx = idx
                , xrdCalibrationEntryNptPath = published </> "calibration" </> printf "XRD18keV_26.nxs_%02d.npt" idx
                }

      entries :: [XRDCalibrationEntry]
      entries = [ entry idx | idx <- idxs]


sampleRef :: XRDRef
sampleRef = XRDRef "reference"
            (published </> "calibration")
            (nxs (published </> "calibration" </> "XRD18keV_26.nxs") "scan_26" h5path')
            6 -- BEWARE only the 6th poni was generated with the right Xpad_flat geometry.

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

bins :: DIM1
bins = ix1 8000

multibins :: Bins
multibins = Bins 25000

threshold :: Threshold
threshold = Threshold 800

ceo2 :: XRDSample
ceo2 = XRDSample "CeO2"
       (published </> "CeO2")
       [ XrdNxs bins multibins threshold n | n <-
         [ nxs (published </> "calibration" </> "XRD18keV_26.nxs") "scan_26" h5path' ]
       ]

n27t2 :: XRDSample
n27t2 = XRDSample "N27T2"
        (published </> "N27T2")
        [ XrdNxs bins multibins threshold n | n <-
          [ nxs (project </> "2016" </> "Run2" </> "2016-03-27" </> "N27T2_14.nxs") "scan_14" h5path
          , nxs (project </> "2016" </> "Run2" </> "2016-03-27" </> "N27T2_17.nxs") "scan_17" h5path
          ]
        ]

r23 :: XRDSample
r23 = XRDSample "R23"
        (published </> "R23")
        [ XrdNxs bins multibins threshold n | n <-
          [ nxs (project </> "2016" </> "Run2" </> "2016-03-27" </> "R23_6.nxs") "scan_6" h5path
          , nxs (project </> "2016" </> "Run2" </> "2016-03-27" </> "R23_12.nxs") "scan_12" h5path
          ]
        ]

r18 :: XRDSample
r18 = XRDSample "R18"
        (published </> "R18")
        [ XrdNxs bins multibins threshold n | n <-
          [ nxs (project </> "2016" </> "Run2" </> "2016-03-27" </> "R18_20.nxs") "scan_20" h5path
          , nxs (project </> "2016" </> "Run2" </> "2016-03-27" </> "R18_24.nxs") "scan_24" h5path
          ]
        ]

a3 :: XRDSample
a3 = XRDSample "A3"
        (published </> "A3")
        [ XrdNxs bins multibins threshold n | n <-
          [ nxs (project </> "2016" </> "Run2" </> "2016-03-27" </> "A3_13.nxs") "scan_13" h5path
          , nxs (project </> "2016" </> "Run2" </> "2016-03-27" </> "A3_14.nxs") "scan_14" h5path
          , nxs (project </> "2016" </> "Run2" </> "2016-03-27" </> "A3_15.nxs") "scan_15" h5path
          ]
        ]

a2 :: XRDSample
a2 = XRDSample "A2"
        (published </> "A2")
        [ XrdNxs bins multibins threshold n | n <-
         [ nxs (project </> "2016" </> "Run2" </> "2016-03-27" </> "A2_14.nxs") "scan_14" h5path
         , nxs (project </> "2016" </> "Run2" </> "2016-03-27" </> "A2_17.nxs") "scan_17" h5path
         ]
        ]

a26 :: XRDSample
a26 = XRDSample "A26"
      (published </> "A26")
      [ XrdNxs bins multibins threshold n | n <-
        [ nxs (project </> "2016" </> "Run2" </> "2016-03-26" </> "A26_50.nxs") "scan_50" h5path
        , nxs (project </> "2016" </> "Run2" </> "2016-03-26" </> "A26_51.nxs") "scan_51" h5path
        , nxs (project </> "2016" </> "Run2" </> "2016-03-26" </> "A26_52.nxs") "scan_52" h5path
        , nxs (project </> "2016" </> "Run2" </> "2016-03-26" </> "A26_53.nxs") "scan_53" h5path
        , nxs (project </> "2016" </> "Run2" </> "2016-03-26" </> "A26_54.nxs") "scan_54" h5path
        , nxs (project </> "2016" </> "Run2" </> "2016-03-26" </> "A26_55.nxs") "scan_55" h5path
        , nxs (project </> "2016" </> "Run2" </> "2016-03-26" </> "A26_56.nxs") "scan_56" h5path
        , nxs (project </> "2016" </> "Run2" </> "2016-03-26" </> "A26_57.nxs") "scan_57" h5path
        , nxs (project </> "2016" </> "Run2" </> "2016-03-26" </> "A26_58.nxs") "scan_58" h5path
        , nxs (project </> "2016" </> "Run2" </> "2016-03-26" </> "A26_59.nxs") "scan_59" h5path
        ]
      ]

d2 :: XRDSample
d2 = XRDSample "D2"
        (published </> "D2")
        [ XrdNxs bins multibins threshold n | n <-
          [ nxs (project </> "2016" </> "Run2" </> "2016-03-27" </> "D2_16.nxs") "scan_16" h5path
          , nxs (project </> "2016" </> "Run2" </> "2016-03-27" </> "D2_17.nxs") "scan_17" h5path
          ]
        ]

d3 :: XRDSample
d3 = XRDSample "D3"
        (published </> "D3")
        [ XrdNxs bins multibins threshold n | n <-
          [ nxs (project </> "2016" </> "Run2" </> "2016-03-27" </> "D3_14.nxs") "scan_14" h5path
          , nxs (project </> "2016" </> "Run2" </> "2016-03-27" </> "D3_15.nxs") "scan_15" h5path
          ]
        ]

f30 :: XRDSample
f30 = XRDSample "F30"
      (published </> "F30")
      [ XrdNxs bins multibins threshold n | n <-
        [ nxs (project </> "2016" </> "Run2" </> "2016-03-26" </> "F30_11.nxs") "scan_11" h5path
        , nxs (project </> "2016" </> "Run2" </> "2016-03-26" </> "F30_12.nxs") "scan_12" h5path
        , nxs (project </> "2016" </> "Run2" </> "2016-03-26" </> "F30_13.nxs") "scan_13" h5path
        ]
      ]

r11 :: XRDSample
r11 = XRDSample "R11"
        (published </> "R11")
        [ XrdNxs bins multibins threshold n | n <-
          [ nxs (project </> "2016" </> "Run2" </> "2016-03-27" </> "R11_5.nxs") "scan_5" h5path
          , nxs (project </> "2016" </> "Run2" </> "2016-03-27" </> "R11_6.nxs") "scan_6" h5path
          , nxs (project </> "2016" </> "Run2" </> "2016-03-27" </> "R11_7.nxs") "scan_7" h5path
          ]
        ]

d16 :: XRDSample
d16 = XRDSample "D16"
        (published </> "D16")
        [ XrdNxs bins multibins threshold n | n <-
          [ nxs (project </> "2016" </> "Run2" </> "2016-03-27" </> "D16_12.nxs") "scan_12" h5path
          , nxs (project </> "2016" </> "Run2" </> "2016-03-27" </> "D16_15.nxs") "scan_15" h5path
          , nxs (project </> "2016" </> "Run2" </> "2016-03-27" </> "D16_17.nxs") "scan_17" h5path
          ]
        ]

k9a2 :: XRDSample
k9a2 = XRDSample "K9A2"
       (published </> "K9A2")
        [ XrdNxs bins multibins threshold n | n <-
          [ nxs (project </> "2016" </> "Run2" </> "2016-03-27" </> "K9A2_1_31.nxs") "scan_31" h5path
          , nxs (project </> "2016" </> "Run2" </> "2016-03-27" </> "K9A2_1_32.nxs") "scan_32" h5path
          ]
        ]

r34n1 :: XRDSample
r34n1 = XRDSample "R34N1"
        (published </> "R34N1")
        [ XrdNxs bins multibins threshold n | n <-
          [ nxs (project </> "2016" </> "Run2" </> "2016-03-27" </> "R34N1_28.nxs") "scan_28" h5path
          , nxs (project </> "2016" </> "Run2" </> "2016-03-27" </> "R34N1_37.nxs") "scan_37" h5path
          ]
        ]

r35n1 :: XRDSample
r35n1 = XRDSample "R35N1"
        (published </> "R35N1")
        [ XrdNxs bins multibins threshold n | n <-
          [ nxs (project </> "2016" </> "Run2" </> "2016-03-26" </> "R35N1_25.nxs") "scan_19" h5path
          , nxs (project </> "2016" </> "Run2" </> "2016-03-26" </> "R35N1_26.nxs") "scan_20" h5path
          , nxs (project </> "2016" </> "Run2" </> "2016-03-26" </> "R35N1_27.nxs") "scan_21" h5path
          ]
        ]

-- | Main

martinetto :: IO ()
martinetto = do
  -- lire le ou les ponis de référence ainsi que leur géométrie
  -- associée.

  -- let samples = [ceo2, a2, a3, a26, d2, d3, d16, f30, k9a2, n27t2, r11, r18, r23, r34n1, r35n1]
  let samples = [ceo2]

  p <- getPoniExtRef sampleRef

  -- flip the ref poni in order to fit the reality
  -- let poniextref = Hkl.PyFAI.PoniExt.flip p
  let poniextref = p
  -- integrate each step of the scan
  _ <- mapConcurrently (integrate poniextref) samples

  -- plot de la figure. (script python ou autre ?)
  return ()

martinetto' :: IO ()
martinetto' = do
  let samples = [ceo2, a2, a3, a26, d2, d3, d16, k9a2, n27t2, r11, r18, r23, r34n1, r35n1]
  -- # need to run f30 by itself because of a segfault in the hkl library
  -- let samples = [f30]
  -- let samples = [ceo2]

  p <- getPoniExtRef sampleRef

  -- flip the ref poni in order to fit the reality
  -- let poniextref = p
  let poniextref = setPose p (MyMatrix HklB (ident 3))
  -- let poniextref = setPose (Hkl.PyFAI.PoniExt.flip p) (MyMatrix HklB (ident 3))

  -- full calibration
  poniextref' <- calibrate sampleCalibration poniextref Xpad32
  -- print p
  print poniextref
  print poniextref'

  -- integrate each step of the scan
  setNumCapabilities 2
  _ <- mapM_ (integrateMulti poniextref') samples
  return ()
