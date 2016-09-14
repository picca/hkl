{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Hkl.PyFAI.Poni
       ( Poni
       , PoniEntry(..)
       , fromAxisAndAngle
       , poniEntryFromList
       , poniEntryToList
       , poniP
       , poniToText
       , flipPoniEntry ) where

import Control.Applicative
import Data.Attoparsec.Text
import Data.Text (Text, append, intercalate, pack)
import Numeric.LinearAlgebra hiding (double)
import Numeric.Units.Dimensional.Prelude (Angle, Length, (+), (*~), (/~), one, micro, meter, radian, degree)

-- | Poni

data PoniEntry = PoniEntry { poniEntryHeader :: [Text]
                           , poniEntryDetector :: (Maybe Text) -- ^ Detector Name
                           , poniEntryPixelSize1 :: (Length Double) -- ^ pixels size 1
                           , poniEntryPixelSize2 :: (Length Double) -- ^ pixels size 1
                           , poniEntryDistance :: (Length Double) -- ^ pixels size 2
                           , poniEntryPoni1 :: (Length Double) -- ^ poni1
                           , poniEntryPoni2 :: (Length Double) -- ^ poni2
                           , poniEntryRot1 :: (Angle Double) -- ^ rot1
                           , poniEntryRot2 :: (Angle Double) -- ^ rot2
                           , poniEntryRot3 :: (Angle Double) -- ^ rot3
                           , poniEntrySpline :: (Maybe Text) -- ^ spline file
                           , poniEntryWavelength :: (Length Double) -- ^ wavelength
                           }
                 deriving (Show)

type Poni = [PoniEntry]

commentP :: Parser Text
commentP =  "#" *> takeTill isEndOfLine <* endOfLine <?> "commentP"

headerP :: Parser [Text]
headerP = many1 commentP <?> "headerP"

doubleP :: Text -> Parser Double
doubleP key = string key *> double <* endOfLine <?> "doubleP"

lengthP :: Text -> Parser (Length Double)
lengthP key = do
  value <-doubleP key
  pure $ value *~ meter

angleP :: Text -> Parser (Angle Double)
angleP key = do
    value <-doubleP key
    pure $ value *~ radian

poniEntryP :: Parser PoniEntry
poniEntryP = PoniEntry
        <$> headerP
        <*> optional ("Detector: " *> takeTill isEndOfLine <* endOfLine)
        <*> lengthP "PixelSize1: "
        <*> lengthP "PixelSize2: "
        <*> lengthP "Distance: "
        <*> lengthP "Poni1: "
        <*> lengthP "Poni2: "
        <*> angleP "Rot1: "
        <*> angleP "Rot2: "
        <*> angleP "Rot3: "
        <*> optional ("SplineFile: " *> takeTill isEndOfLine <* endOfLine)
        <*> lengthP "Wavelength: "
        <?> "poniEntryP"

poniP :: Parser Poni
poniP = many poniEntryP

poniToText :: Poni -> Text
poniToText p = Data.Text.intercalate (Data.Text.pack "\n") (map poniEntryToText p)

poniEntryFromList :: [Double] -> Length Double -> PoniEntry
poniEntryFromList [rot1, rot2, rot3, d, poni1, poni2] w =
    (PoniEntry h md p1 p2 d' pon1 pon2 r1 r2 r3 ms w)
        where
          h = [Data.Text.pack ""]
          md = Just (Data.Text.pack "xpad_flat")
          p1 = 130 *~ micro meter
          p2 = 130 *~ micro meter
          pon1 = poni1 *~ meter
          pon2 = poni2 *~ meter
          d' = d *~ meter
          r1 = rot1 *~ radian
          r2 = rot2 *~ radian
          r3 = rot3 *~ radian
          ms = Nothing
poniEntryFromList _ _ = error "Can not convert to a PoniEntry" 

poniEntryToList :: PoniEntry -> ([Double], Length Double)
poniEntryToList p = ( [ poniEntryRot1 p /~ radian
                      , poniEntryRot2 p /~ radian
                      , poniEntryRot3 p /~ radian
                      , poniEntryDistance p /~ meter
                      , poniEntryPoni1 p /~ meter
                      , poniEntryPoni2 p /~ meter
                      ]
                    , poniEntryWavelength p
                    )

poniEntryToText :: PoniEntry -> Text
poniEntryToText (PoniEntry h md p1 p2 d poni1 poni2 rot1 rot2 rot3 ms w) =
  intercalate (Data.Text.pack "\n") $
    map (Data.Text.append "#") h
    ++ maybe [] (poniLine' "Detector: ") md
    ++ poniLine "PixelSize1: " (p1 /~ meter)
    ++ poniLine "PixelSize2: " (p2 /~ meter)
    ++ poniLine "Distance: " (d /~ meter)
    ++ poniLine "Poni1: " (poni1 /~ meter)
    ++ poniLine "Poni2: " (poni2 /~ meter)
    ++ poniLine "Rot1: " (rot1 /~ radian)
    ++ poniLine "Rot2: " (rot2 /~ radian)
    ++ poniLine "Rot3: " (rot3 /~ radian)
    ++ maybe [] (poniLine' "SplineFile: ") ms
    ++ poniLine "Wavelength: " (w /~ meter)
  where
    poniLine :: Show a => String -> a -> [Text]
    poniLine key v = [Data.Text.append (Data.Text.pack key) (Data.Text.pack $ show v)]

    poniLine' :: String -> Text -> [Text]
    poniLine' key v = [Data.Text.append (Data.Text.pack key) v]

crossprod :: Vector Double -> Matrix Double
crossprod axis = fromLists [[ 0, -z,  y],
                            [ z,  0, -x],
                            [-y,  x,  0]]
    where
      x = axis `atIndex` 0
      y = axis `atIndex` 1
      z = axis `atIndex` 2

fromAxisAndAngle :: Vector Double -> Angle Double -> Matrix Double
fromAxisAndAngle axis angle = ident 3 Prelude.+ s * q Prelude.+ c * (q <> q)
    where
      c = scalar (1 - cos (angle /~ one))
      s = scalar (sin (angle /~ one))
      q = crossprod axis

flipPoniEntry :: PoniEntry -> PoniEntry
flipPoniEntry (PoniEntry header detector px1 px2 distance poni1 poni2 rot1 rot2 rot3 spline wavelength) =
  PoniEntry header detector px1 px2 distance poni1 poni2 rot1 rot2 new_rot3 spline wavelength
  where
    new_rot3 = rot3 Numeric.Units.Dimensional.Prelude.+ 180 *~ degree
