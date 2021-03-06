{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Hkl.PyFAI.Poni
       ( Poni
       , PoniEntry(..)
       , fromAxisAndAngle
       , poniP
       , poniToText
       , flipPoniEntry
       , poniEntryRotation
       , poniEntryTranslation
       ) where

import Control.Applicative
import Data.Attoparsec.Text
import Data.Text (Text, append, intercalate, pack)
import Numeric.LinearAlgebra hiding (double)
import Numeric.Units.Dimensional.Prelude (Angle, Length, (+), (*~), (/~), (/~~), one, meter, radian, degree)

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

poniEntryToText :: PoniEntry -> Text
poniEntryToText p = intercalate (Data.Text.pack "\n") $
                    map (Data.Text.append "#") (poniEntryHeader p)
                    ++ maybe [] (poniLine' "Detector: ") (poniEntryDetector p)
                    ++ poniLine "PixelSize1: " (poniEntryPixelSize1 p /~ meter)
                    ++ poniLine "PixelSize2: " (poniEntryPixelSize2 p /~ meter)
                    ++ poniLine "Distance: " (poniEntryDistance p /~ meter)
                    ++ poniLine "Poni1: " (poniEntryPoni1 p /~ meter)
                    ++ poniLine "Poni2: " (poniEntryPoni2 p /~ meter)
                    ++ poniLine "Rot1: " (poniEntryRot1 p /~ radian)
                    ++ poniLine "Rot2: " (poniEntryRot2 p /~ radian)
                    ++ poniLine "Rot3: " (poniEntryRot3 p /~ radian)
                    ++ maybe [] (poniLine' "SplineFile: ") (poniEntrySpline p)
                    ++ poniLine "Wavelength: " (poniEntryWavelength p /~ meter)
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
flipPoniEntry p = p { poniEntryRot3 = new_rot3 }
  where
    rot3 = poniEntryRot3 p
    new_rot3 = rot3 Numeric.Units.Dimensional.Prelude.+ 180 *~ degree

poniEntryRotation :: PoniEntry ->  Matrix Double -- TODO MyMatrix PyFAIB
poniEntryRotation e = Prelude.foldl (<>) (ident 3) rotations
    where
      rot1 = poniEntryRot1 e
      rot2 = poniEntryRot2 e
      rot3 = poniEntryRot3 e
      rotations = Prelude.map (uncurry fromAxisAndAngle)
                  [ (fromList [0, 0, 1], rot3)
                  , (fromList [0, 1, 0], rot2)
                  , (fromList [1, 0, 0], rot1)]

poniEntryTranslation :: PoniEntry -> Vector Double
poniEntryTranslation e = fromList ( [ poniEntryPoni1 e
                                    , poniEntryPoni2 e
                                    , poniEntryDistance e
                                    ] /~~ meter )
