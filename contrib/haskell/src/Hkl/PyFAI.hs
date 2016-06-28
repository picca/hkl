{-# LANGUAGE OverloadedStrings #-}

module Hkl.PyFAI
       ( poniP ) where

import Control.Applicative
import Data.Attoparsec.Text
import Data.Text
import Hkl.Types
-- import Numeric.Units.Dimensional.Prelude (Length, Angle)

eol :: Char -> Bool
eol c = c == '\r' || c == '\n'

commentP :: Parser Text
commentP =  "#" *> takeWhile1 eol

headerP :: Parser [Text]
headerP = many commentP


doubleP :: Text -> Parser Double
doubleP key = string key *> double

poniP :: Parser Poni
poniP = Poni
        <$> headerP
        <*> ("Detector:" *> takeWhile1 eol)
        <*> doubleP "PixelSize1:"
        <*> doubleP "PixelSize2:"
        <*> doubleP "Distance:"
        <*> doubleP "Poni1:"
        <*> doubleP "Poni2:"
        <*> doubleP "Rot1:"
        <*> doubleP "Rot2:"
        <*> doubleP "Rot3:"
        <*> doubleP "Wavelength:"
