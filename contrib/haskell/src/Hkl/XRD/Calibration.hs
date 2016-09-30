{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Hkl.XRD.Calibration
       ( NptExt(..)
       , XRDCalibrationEntry(..)
       , XRDCalibration(..)
       , calibrate
       ) where

import Control.Monad.IO.Class
import Data.ByteString.Char8
import Data.List
import Data.Text
import Data.Vector.Storable
import Hkl.C
import Hkl.Detector
import Hkl.H5
import Hkl.PyFAI
import Hkl.MyMatrix
import Hkl.PyFAI.PoniExt
import Hkl.Types
import Hkl.XRD
import Numeric.LinearAlgebra
import Numeric.GSL.Minimization
-- import Graphics.Plot
import Numeric.Units.Dimensional.Prelude (meter, radian, nano, (/~), (*~))
import Prelude hiding (concat, lookup, readFile, writeFile, unlines)

import Pipes.Safe (MonadSafe(..), runSafeT, bracket)

#if !MIN_VERSION_hmatrix(0, 17, 0)
(#>) :: Matrix Double -> Vector Double -> Vector Double
(#>) = (<>)
#endif

-- | Calibration

data NptExt a = NptExt { nptExtNpt :: Npt
                       , nptExtMyMatrix :: MyMatrix Double
                       , nptExtDetector :: Detector a
                       }
              deriving (Show)

data XRDCalibrationEntry = XRDCalibrationEntry { xrdCalibrationEntryNxs :: Nxs
                                               , xrdCalibrationEntryIdx :: Int
                                               , xrdCalibrationEntryNptPath :: FilePath
                                               }
                           deriving (Show)

data XRDCalibration = XRDCalibration { xrdCalibrationName :: Text
                                     , xrdCalibrationOutputDir :: FilePath
                                     , xrdCalibrationEntries :: [XRDCalibrationEntry]
                                     }
                      deriving (Show)

withDataItem :: MonadSafe m => File -> DataItem -> (Dataset -> m r) -> m r
withDataItem hid (DataItem name _) = Pipes.Safe.bracket (liftIO acquire') (liftIO . release')
    where
      acquire' :: IO Dataset
      acquire' = openDataset hid (Data.ByteString.Char8.pack name) Nothing

      release' :: Dataset -> IO ()
      release' = closeDataset

getM :: File -> DataFrameH5Path -> Int -> IO (MyMatrix Double)
getM f p i' = runSafeT $
    withDataItem f (h5pGamma p) $ \g' ->
    withDataItem f (h5pDelta p) $ \d' ->
    withDataItem f (h5pWavelength p) $ \w' -> liftIO $ do
      let mu = 0.0
      let komega = 0.0
      let kappa = 0.0
      let kphi = 0.0
      gamma <- get_position g' 0
      delta <- get_position d' i'
      wavelength <- get_position w' 0
      let source = Source (Data.Vector.Storable.head wavelength *~ nano meter)
      let positions = Data.Vector.Storable.concat [mu, komega, kappa, kphi, gamma, delta]
      let geometry = Geometry K6c source positions Nothing
      let detector = ZeroD
      m <- geometryDetectorRotationGet geometry detector
      return (MyMatrix HklB m)

readXRDCalibrationEntry :: Detector a -> XRDCalibrationEntry -> IO (NptExt a)
readXRDCalibrationEntry d e =
    withH5File f $ \h5file -> do
      m <- getM h5file p idx
      npt <- nptFromFile (xrdCalibrationEntryNptPath e)
      return (NptExt npt m d)
    where
      idx = xrdCalibrationEntryIdx e
      (Nxs f _ p) = xrdCalibrationEntryNxs e

-- | Poni Calibration

-- The minimized function is the quadratic difference of the
-- theoretical tth angle and for each pixel, the computed tth angle.

-- synonyme types use in order to improve the calibration performance

type NptEntry' = (Double, [Vector Double]) -- tth, detector pixels coordinates
type Npt' = (Double, [NptEntry']) -- wavelength, [NptEntry']
type NptExt' a = (Npt', Matrix Double, Detector a)

poniEntryFromList :: PoniEntry -> [Double] -> PoniEntry
poniEntryFromList p [rot1, rot2, rot3, poni1, poni2, d] =
  p { poniEntryDistance = d *~ meter
    , poniEntryPoni1 = poni1 *~ meter
    , poniEntryPoni2 = poni2 *~ meter
    , poniEntryRot1 = rot1 *~ radian
    , poniEntryRot2 = rot2 *~ radian
    , poniEntryRot3 = rot3 *~ radian
    }
poniEntryFromList _ _ = error "Can not convert to a PoniEntry"

poniEntryToList :: PoniEntry -> [Double]
poniEntryToList p = [ poniEntryRot1 p /~ radian
                    , poniEntryRot2 p /~ radian
                    , poniEntryRot3 p /~ radian
                    , poniEntryPoni1 p /~ meter
                    , poniEntryPoni2 p /~ meter
                    , poniEntryDistance p /~ meter
                    ]

calibrate :: XRDCalibration -> PoniExt -> Detector a -> IO PoniExt
calibrate c (PoniExt p _) d =  do
  let entry = Prelude.last p
  let guess = Data.Vector.Storable.fromList $ poniEntryToList entry
  -- read all the NptExt
  npts <- Prelude.mapM (readXRDCalibrationEntry d) (xrdCalibrationEntries c)
  -- in order to improve computation speed, pre-compute the pixel coodinates.

  let (solution, _p) = minimizeV NMSimplex2 1E-16 3000 box (f (preCalibrate npts)) guess
  -- mplot $ drop 3 (toColumns p)
  print _p
  return $ PoniExt [poniEntryFromList entry (Data.Vector.Storable.toList solution)] (MyMatrix HklB (ident 3))
    where
      preCalibrate''' :: Detector a -> NptEntry -> NptEntry'
      preCalibrate''' detector (NptEntry _ tth _ points) = (tth /~ radian, Prelude.map (coordinates detector) points)

      preCalibrate'' :: Npt -> Detector a -> Npt'
      preCalibrate'' n detector = (nptWavelength n /~ meter, Prelude.map (preCalibrate''' detector) (nptEntries n))

      preCalibrate' :: NptExt a -> NptExt' a
      preCalibrate' (NptExt n m detector) = (preCalibrate'' n detector, m', detector)
        where
          (MyMatrix _ m') = changeBase m PyFAIB

      preCalibrate :: [NptExt a] -> [NptExt' a]
      preCalibrate ns = Prelude.map preCalibrate' ns

      box :: Vector Double
      box = Data.Vector.Storable.fromList [0.1, 0.1, 0.1, 0.01, 0.01, 0.01]

      f :: [NptExt' a] -> Vector Double -> Double
      f ns params = Data.List.foldl' (f' rotation translation) 0 ns
        where
            rot1 = params `atIndex` 0
            rot2 = params `atIndex` 1
            rot3 = params `atIndex` 2

            rotations = Prelude.map (uncurry fromAxisAndAngle)
                        [ (Data.Vector.Storable.fromList [0, 0, 1], rot3 *~ radian)
                        , (Data.Vector.Storable.fromList [0, 1, 0], rot2 *~ radian)
                        , (Data.Vector.Storable.fromList [1, 0, 0], rot1 *~ radian)]

            rotation = Data.List.foldl' (<>) (ident 3) rotations

            translation :: Vector Double
            translation = slice 3 3 params

      f' :: Matrix Double -> Vector Double -> Double -> NptExt' a -> Double
      f' rotation translation x ((_wavelength, entries), m, _detector) =
        Data.List.foldl' (f'' translation r) x entries
          where
            r :: Matrix Double
            r = m <> rotation

      f'' :: Vector Double -> Matrix Double -> Double -> NptEntry'-> Double
      {-# INLINE f'' #-}
      f'' translation r x (tth, pixels) = Data.List.foldl' (f''' translation r tth) x pixels

      f''' :: Vector Double -> Matrix Double -> Double -> Double -> Vector Double -> Double
      {-# INLINE f''' #-}
      f''' translation r tth x pixel = x + dtth * dtth
          where
            kf = r #> (pixel - translation)
            x' = kf `atIndex` 0
            y' = kf `atIndex` 1
            z' = kf `atIndex` 2

            dtth = tth - atan2 (sqrt (x'*x' + y'*y')) (-z')
