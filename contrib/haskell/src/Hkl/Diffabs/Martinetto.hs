module Hkl.Diffabs.Martinetto
       ( main_martinetto ) where

import Control.Monad
import Data.Attoparsec.Text
{-# LANGUAGE OverloadedStrings #-}
import Data.Either
import Data.List (sort)
import Data.Text (Text, intercalate, pack)
import Data.Text.IO
import Hkl.Types
import Hkl.PyFAI
import Numeric.Units.Dimensional.Prelude (meter, degree, (/~))
import System.FilePath
import System.FilePath.Glob
import System.IO (withFile, IOMode(WriteMode) )

import Prelude hiding (lookup, readFile)


-- import Graphics.Rendering.Chart.Easy
-- import Graphics.Rendering.Chart.Backend.Diagrams

-- plotPonies :: FilePath -> [PoniEntry] -> IO ()
-- plotPonies f entries = toFile def f $ do
--     layout_title .= "Ponies"
--     setColors [opaque blue]
--     let values = map extract entries
--     plot (line "am" [values [0,(0.5)..400]])
--     -- plot (points "am points" (signal [0,7..400]))
--     where
--       extract (PoniEntry _ _ (Length poni1) _ _ _ _ _ _) = poni1

title :: Text
title = intercalate (pack "\t") (map pack [ "# distance"
                                          , "poni1"
                                          , "poni2"
                                          , "rot1"
                                          , "rot2"
                                          , "rot3" ])

toText :: PoniEntry -> Text
toText (PoniEntry _ _ _ _ d p1 p2 rot1 rot2 rot3 _ _) =
  intercalate (pack "\t") (map (pack . show) [ d /~ meter
                                             , p1 /~ meter
                                             , p2 /~ meter
                                             , rot1 /~ degree
                                             , rot2 /~ degree
                                             , rot3 /~ degree])



save :: FilePath -> [PoniEntry] -> IO ()
save f ps = do
  withFile f WriteMode $ \handler -> do
    hPutStrLn handler title
    mapM_ (put handler) ps
    where
      put h p = hPutStrLn h (toText p)

ponies :: [FilePath] -> IO [PoniEntry]
ponies fs = mapM extract (sort fs)
  where
     extract :: FilePath -> IO PoniEntry
     extract filename = do
       content <- readFile filename
       return $ case (parseOnly poniP content) of
         Left _ -> error $ "Can not parse the " ++ filename ++ " poni file"
         Right poni -> last poni

main_martinetto :: IO ()
main_martinetto = do
  let project = "/nfs/ruche-diffabs/diffabs-users/99160066/"
  let published = project </> "published-data"
  let calibration = published </> "calibration"
  let output = calibration </> "ponies.txt"
  -- let filename = "/home/picca/tmp/reguer/rocha/merged.poni"
  -- let filename = "../cirpad/blender/test2.poni"
  filenames <- glob $ calibration </> "XRD18keV_26*.poni"
  entries <- ponies filenames
  save output entries
