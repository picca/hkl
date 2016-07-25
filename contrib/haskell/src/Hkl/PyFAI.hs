{-# LANGUAGE OverloadedStrings #-}

module Hkl.PyFAI
       ( poniP
       , rotatePoniEntry ) where

import Control.Applicative
import Data.Attoparsec.Text
import Data.Text (Text)
import Hkl.Types
import Numeric.LinearAlgebra ( Matrix, Vector
                             , ident, fromList, fromLists
                             , scalar, trans
                             , (@>), (@@>), (<>))
import Numeric.Units.Dimensional.Prelude (Angle, Length, (*~), (/~), one, meter, radian)

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


crossprod :: Vector Double -> Matrix Double
crossprod axis = fromLists [[ 0, -z,  y],
                            [ z,  0, -x],
                            [-y,  x,  0]]
    where
      x = axis @> 0
      y = axis @> 1
      z = axis @> 2


fromAxisAndAngle :: Vector Double -> Angle Double -> Matrix Double
fromAxisAndAngle axis angle = ident 3 + s * q + c * (q <> q)
    where
      c = scalar (1 - cos (angle /~ one))
      s = scalar (sin (angle /~ one))
      q = crossprod axis


toEulerians :: Matrix Double -> (Angle Double, Angle Double, Angle Double)
toEulerians m
  | abs c > epsilon = ( atan2 ((m @@> (2, 1)) / c) ((m @@> (2, 2)) / c) *~ radian
                      , rot2 *~ radian
                      , atan2 ((m @@> (1, 0)) / c) ((m @@> (0, 0)) / c) *~ radian
                      )
  | otherwise        = ( 0 *~ radian
                       , rot2 *~ radian
                       , atan2 (-(m @@> (0, 1))) (m @@> (1, 1)) *~ radian
                       )
  where
    epsilon = 1e-10
    rot2 = asin (-(m @@> (2, 0)))
    c = cos rot2


rotatePoniEntry :: PoniEntry -> Matrix Double -> Matrix Double -> PoniEntry
rotatePoniEntry (PoniEntry header detector px1 px2 distance poni1 poni2 rot1 rot2 rot3 spline wavelength) m1 m2 = PoniEntry header detector px1 px2 distance poni1 poni2 new_rot1 new_rot2 new_rot3 spline wavelength
  where
    rotations = map (uncurry fromAxisAndAngle)
                [ (fromList [0, 0, 1], rot3)
                , (fromList [0, 1, 0], rot2)
                , (fromList [1, 0, 0], rot1)]
    -- M1 . R0 = R1
    r1 = foldl (<>) (ident 3) rotations
    -- M2 . R0 = R2
    -- R2 = M2 . M1.T . R1
    r2 = foldl (<>) m2 [trans m1, r1]
    (new_rot1, new_rot2, new_rot3) = toEulerians r2
