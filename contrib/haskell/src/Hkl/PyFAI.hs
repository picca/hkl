{-# LANGUAGE OverloadedStrings #-}

module Hkl.PyFAI
       ( poniP ) where

import Control.Applicative
import Data.Attoparsec.Text
import Data.Text (Text)
import Hkl.Types
-- import Numeric.Units.Dimensional.Prelude (Length, Angle)

commentP :: Parser Text
commentP =  "#" *> takeTill isEndOfLine <* endOfLine <?> "commentP"

headerP :: Parser [Text]
headerP = count 1 commentP <?> "headerP"

doubleP :: Text -> Parser Double
doubleP key = string key *> double <* endOfLine <?> "doubleP"

poniEntryP :: Parser PoniEntry
poniEntryP = PoniEntry
        <$> headerP
        <*> optional ("Detector: " *> takeTill isEndOfLine <* endOfLine)
        <*> doubleP "PixelSize1: "
        <*> doubleP "PixelSize2: "
        <*> doubleP "Distance: "
        <*> doubleP "Poni1: "
        <*> doubleP "Poni2: "
        <*> doubleP "Rot1: "
        <*> doubleP "Rot2: "
        <*> doubleP "Rot3: "
        <*> optional ("SplineFile: " *> takeTill isEndOfLine <* endOfLine)
        <*> doubleP "Wavelength: "
        <?> "poniEntryP"

poniP :: Parser Poni
poniP = many' poniEntryP
