{-# LANGUAGE OverloadedStrings #-}

module Hkl.PyFAI.Npt
       ( Npt(..)
       , NptEntry(..)
       , NptPoint(..)
       , nptP
       , nptFromFile
       ) where

import Control.Applicative
import Data.Attoparsec.Text
import Data.Text
import Data.Text.IO (readFile)
import Numeric.Units.Dimensional.Prelude (Angle, Length, (*~), meter, radian)

type Calibrant = Text

data NptPoint = NptPoint { nptPointX :: Double
                         , nptPointY :: Double
                         }
              deriving (Show)

data NptEntry = NptEntry { nptEntryId :: Int
                         , nptEntryTth :: Angle Double
                         , nptEntryRing :: Int
                         , nptPoints :: [NptPoint]
                         }
              deriving (Show)

data Npt = Npt { nptComment :: [Text]
               , nptCalibrant :: Calibrant
               , nptWavelength :: Length Double
               , npdDSpacing :: [Length Double]
               , nptEntries :: [NptEntry]
               }
           deriving (Show)

commentP :: Parser Text
commentP =  "#" *> takeTill isEndOfLine <* endOfLine <?> "commentP"

headerP :: Parser [Text]
headerP = many1 commentP <?> "headerP"

calibrantP :: Parser Text
calibrantP = "calibrant: " *> takeTill isEndOfLine <* endOfLine <?> "calibrantP"

dspacingP :: Parser [Length Double]
dspacingP = "dspacing:" *> many1 lengthP' <* endOfLine <?> "dspasingP"

doubleP :: Text -> Parser Double
doubleP key = string key *> double <* endOfLine <?> "doubleP"

lengthP' :: Parser (Length Double)
lengthP' = do
  skipSpace
  value <- double
  pure $ value *~ meter

lengthP :: Text -> Parser (Length Double)
lengthP key = do
  value <- doubleP key
  pure $ value *~ meter

angleP :: Text -> Parser (Angle Double)
angleP key = do
  value <-doubleP key
  pure $ value *~ radian

intP :: Text -> Parser Int
intP key = string key *> decimal <* endOfLine <?> "intP"

nptPointP :: Parser NptPoint
nptPointP = NptPoint
  <$> ("point: x=" *> double)
  <*> (" y=" *> double <* endOfLine)

nptEntryP :: Parser NptEntry
nptEntryP = NptEntry
            <$> (skipSpace *> intP "New group of points: ")
            <*> angleP "2theta: "
            <*> intP "ring: "
            <*> many nptPointP

nptP :: Parser Npt
nptP = Npt
       <$> headerP
       <*> calibrantP
       <*> lengthP "wavelength: "
       <*> dspacingP
       <*> many1 nptEntryP
       <?> "nptP"

nptFromFile :: FilePath -> IO Npt
nptFromFile filename = do
  content <- Data.Text.IO.readFile filename
  return $ case parseOnly nptP content of
    Left _     -> error $ "Can not parse the " ++ filename ++ " npt file"
    Right a -> a
