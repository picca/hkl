{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Hkl.Diffabs.Melle
       ( melle ) where

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

melle :: IO ()
melle = return ()

-- project :: FilePath
-- project = "/nfs/ruche-diffabs/diffabs-users/99160066/"

-- published :: FilePath
-- published = project </> "published-data"

-- beamlineUpper :: Beamline -> String
-- beamlineUpper b = [Data.Char.toUpper x | x <- show b]

-- nxs :: FilePath -> NxEntry -> (NxEntry -> DataFrameH5Path) -> Nxs
-- nxs f e h = Nxs f e (h e)

-- nxs' :: FilePath -> NxEntry -> (NxEntry -> a) -> Nxs' a
-- nxs' f e h = Nxs' f e (h e)

-- h5path :: NxEntry -> DataFrameH5Path
-- h5path nxentry =
--     DataFrameH5Path { h5pImage = DataItem (nxentry </> image) StrictDims
--                     , h5pGamma = DataItem (nxentry </> beamline </> gamma) ExtendDims
--                     , h5pDelta = DataItem (nxentry </> delta) ExtendDims
--                     , h5pWavelength = DataItem (nxentry </> beamline </> wavelength) StrictDims
--                     }
--         where
--           beamline :: String
--           beamline = beamlineUpper Diffabs

--           image = "scan_data/data_53"
--           gamma = "d13-1-cx1__EX__DIF.1-GAMMA__#1/raw_value"
--           delta = "scan_data/actuator_1_1"
--           wavelength = "D13-1-C03__OP__MONO__#1/wavelength"

-- sampleCalibration :: XRDCalibration
-- sampleCalibration = XRDCalibration { xrdCalibrationName = "calibration"
--                                    , xrdCalibrationOutputDir = published </> "calibration"
--                                    , xrdCalibrationEntries = entries
--                                    }
--     where

--       idxs :: [Int]
--       idxs = [3, 6, 9, 15, 18, 21, 24, 27, 30, 33, 36, 39, 43]

--       entry :: Int -> XRDCalibrationEntry
--       entry idx = XRDCalibrationEntryNxs
--                 { xrdCalibrationEntryNxs'Nxs = nxs (published </> "calibration" </> "XRD18keV_26.nxs") "scan_26" h5path
--                 , xrdCalibrationEntryNxs'Idx = idx
--                 , xrdCalibrationEntryNxs'NptPath = published </> "calibration" </> printf "XRD18keV_26.nxs_%02d.npt" idx
--                 }

--       entries :: [XRDCalibrationEntry]
--       entries = [ entry idx | idx <- idxs]


-- sampleRef :: XRDRef
-- sampleRef = XRDRef "reference"
--             (published </> "calibration")
--             (nxs (published </> "calibration" </> "XRD18keV_26.nxs") "scan_26" h5path)
--             6 -- BEWARE only the 6th poni was generated with the right Xpad_flat geometry.

-- h5path' :: NxEntry -> DataFrameMeshH5Path
-- h5path' nxentry =
--     DataFrameMeshH5Path { dataFrameMeshH5Path'Image = DataItem (nxentry </> image) StrictDims
--                         , dataFrameMeshH5Path'MeshX = DataItem (nxentry </> meshX) StrictDims
--                         , dataFrameMeshH5Path'MeshY = DataItem (nxentry </> meshY) StrictDims
--                         , dataFrameMeshH5Path'Gamma = DataItem (nxentry </> beamline </> gamma) ExtendDims
--                         , dataFrameMeshH5Path'Delta = DataItem (nxentry </> beamline </> delta) ExtendDims
--                         , dataFrameMeshH5Path'Wavelength = DataItem (nxentry </> beamline </> wavelength) StrictDims
--                         }
--   where
--     beamline :: String
--     beamline = beamlineUpper Diffabs

--     image = "scan_data/data_58"
--     meshX = "scan_data/actuator_1_1"
--     meshY = "scan_data/actuator_2_1"
--     gamma = "D13-1-CX1__EX__DIF.1-GAMMA__#1/raw_value"
--     delta = "D13-1-CX1__EX__DIF.1-DELTA__#1/raw_value"
--     wavelength = "D13-1-C03__OP__MONO__#1/wavelength"

-- bins :: DIM1
-- bins = ix1 8000

-- multibins :: DIM1
-- multibins = ix1 25000

-- threshold :: Threshold
-- threshold = Threshold 800

-- melle29 :: XRDSample' DataFrameMeshH5Path
-- melle29 = XRDSample' "MELLE_29"
--           (published </> "MELLE_29")
--           [ XrdNxs' bins multibins threshold n | n <-
--             [ nxs' (project </> "2016" </> "Run2" </> "2016-03-28" </> "MELLE_29.nxs") "scan_29" h5path'
--             ]
--           ]

-- -- meshSample :: String
-- -- meshSample = project </> "2016" "Run2" "2016-03-28" "MELLE_29.nxs"
-- -- scan_29 scan_data actuator_1_1 actuator_2_1 data_58 (images)

-- -- | Main

-- martinetto' :: IO ()
-- martinetto' = do
--   let samples = [melle29]

--   p <- getPoniExtRef sampleRef

--   -- flip the ref poni in order to fit the reality
--   -- let poniextref = p
--   let poniextref = setPose p (MyMatrix HklB (ident 3))
--   -- let poniextref = setPose (Hkl.PyFAI.PoniExt.flip p) (MyMatrix HklB (ident 3))

--   -- full calibration
--   poniextref' <- calibrate sampleCalibration poniextref Xpad32
--   -- print p
--   print poniextref
--   print poniextref'

--   -- integrate each step of the scan
--   _ <- mapM_ (integrateMesh poniextref') samples

--   return ()
