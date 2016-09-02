{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Hkl.PyFAI
       ( poniP
       , poniToText
       , rotatePoniEntry
       , flipPoniEntry ) where

import Control.Applicative
import Data.Attoparsec.Text
import Data.Text (Text, append, intercalate, pack)
import Hkl.Types
import Numeric.LinearAlgebra hiding (double)
import Numeric.Units.Dimensional.Prelude (Angle, Length, (+), (*~), (/~), one, meter, radian, degree)

#if !MIN_VERSION_hmatrix(0, 17, 0)
tr:: Matrix t -> Matrix t
tr = trans
#endif

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


toEulerians :: Matrix Double -> (Angle Double, Angle Double, Angle Double)
toEulerians m
  | abs c > epsilon = ( atan2 ((m `atIndex` (2, 1)) / c) ((m `atIndex` (2, 2)) / c) *~ radian
                      , rot2 *~ radian
                      , atan2 ((m `atIndex` (1, 0)) / c) ((m `atIndex` (0, 0)) / c) *~ radian
                      )
  | otherwise        = ( 0 *~ radian
                       , rot2 *~ radian
                       , atan2 (-(m `atIndex` (0, 1))) (m `atIndex` (1, 1)) *~ radian
                       )
  where
    epsilon = 1e-10
    rot2 = asin (-(m `atIndex` (2, 0)))
    c = cos rot2


rotatePoniEntry :: PoniEntry -> MyMatrix Double -> MyMatrix Double -> PoniEntry
rotatePoniEntry (PoniEntry header detector px1 px2 distance poni1 poni2 rot1 rot2 rot3 spline wavelength) mym1 mym2 = PoniEntry header detector px1 px2 distance poni1 poni2 new_rot1 new_rot2 new_rot3 spline wavelength
  where
    rotations = map (uncurry fromAxisAndAngle)
                [ (fromList [0, 0, 1], rot3)
                , (fromList [0, 1, 0], rot2)
                , (fromList [1, 0, 0], rot1)]
    -- M1 . R0 = R1
    r1 = foldl (<>) (ident 3) rotations -- pyFAIB
    -- M2 . R0 = R2
    -- R2 = M2 . M1.T . R1
    r2 = foldl (<>) m2 [tr m1, r1]
    (new_rot1, new_rot2, new_rot3) = toEulerians r2

    changeBase :: MyMatrix Double -> Basis -> MyMatrix Double
    changeBase (MyMatrix PyFAIB m) HklB = MyMatrix HklB (passage m p2)
    changeBase (MyMatrix HklB m) PyFAIB = MyMatrix PyFAIB (passage m p1)
    changeBase m@(MyMatrix PyFAIB _) PyFAIB = m
    changeBase m@(MyMatrix HklB _) HklB = m

    passage :: Matrix Double -> Matrix Double -> Matrix Double
    passage r p = inv p <> r <> p

    p1 :: Matrix Double -- hkl -> pyFAI
    p1 = fromLists [ [0,  0, -1]
                   , [0, -1,  0]
                   , [1,  0,  0]]

    p2 :: Matrix Double -- pyFAI -> hkl:
    p2 = fromLists [ [ 0,  0, 1]
                   , [ 0, -1, 0]
                   , [-1,  0, 0]]

    (MyMatrix _ m1) = changeBase mym1 PyFAIB
    (MyMatrix _ m2) = changeBase mym2 PyFAIB

flipPoniEntry :: PoniEntry -> PoniEntry
flipPoniEntry (PoniEntry header detector px1 px2 distance poni1 poni2 rot1 rot2 rot3 spline wavelength) =
  PoniEntry header detector px1 px2 distance poni1 poni2 rot1 rot2 new_rot3 spline wavelength
  where
    new_rot3 = rot3 Numeric.Units.Dimensional.Prelude.+ 180 *~ degree
