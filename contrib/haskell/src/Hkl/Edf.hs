{-# LANGUAGE OverloadedStrings #-}

module Hkl.Edf
       ( Edf(..)
       , edfP
       , edfFromFile
       ) where

import Control.Applicative
import Data.Attoparsec.Text
import Data.ByteString.Char8 (readFile, split)
import Data.Text (Text, words)
import Data.Text.Encoding (decodeUtf8)
import Numeric.Units.Dimensional.Prelude (Length, (*~), nano, meter)

import Prelude hiding (readFile, words)

data Edf = Edf { edf'Lambda :: Length Double
               , edf'MotorPos :: [Double]
               , edf'MotorMne :: [Text]
               }
         deriving (Show)

-- commentP :: Parser Text
-- commentP =  "#" *> takeTill isEndOfLine <* endOfLine <?> "commentP"

-- headerP :: Parser [Text]
-- headerP = many1 commentP <?> "headerP"

-- calibrantP :: Parser Text
-- calibrantP = "calibrant: " *> takeTill isEndOfLine <* endOfLine <?> "calibrantP"

-- dspacingP :: Parser [Length Double]
-- dspacingP = "dspacing:" *> many1 lengthP' <* endOfLine <?> "dspasingP"

-- doubleP :: Text -> Parser Double
-- doubleP key = string key *> double <* endOfLine <?> "doubleP"

-- lengthP' :: Parser (Length Double)
-- lengthP' = do
--   skipSpace
--   value <- double
--   pure $ value *~ meter

-- lengthP :: Text -> Parser (Length Double)
-- lengthP key = do
--   value <- doubleP key
--   pure $ value *~ meter

-- angleP :: Text -> Parser (Angle Double)
-- angleP key = do
--   value <-doubleP key
--   pure $ value *~ radian

-- intP :: Text -> Parser Int
-- intP key = string key *> decimal <* endOfLine <?> "intP"

-- nptPointP :: Parser NptPoint
-- nptPointP = NptPoint
--   <$> ("point: x=" *> double)
--   <*> (" y=" *> double <* endOfLine)

-- nptEntryP :: Parser NptEntry
-- nptEntryP = NptEntry
--             <$> (skipSpace *> intP "New group of points: ")
--             <*> angleP "2theta: "
--             <*> intP "ring: "
--             <*> many nptPointP

edf'LambdaP :: Parser (Length Double)
edf'LambdaP = do
  _ <- manyTill anyChar (try $ string "Lambda = ")
  value <- double
  pure $ value *~ nano meter

edf'MotorPosP :: Parser [Double]
edf'MotorPosP = do
  _ <- manyTill anyChar (try $ string "motor_pos = ")
  many1 (skipSpace *> double)

edf'MotorMneP :: Parser [Text]
edf'MotorMneP = do
  _ <- manyTill anyChar (try $ string "motor_mne = ")
  vs <- takeTill (\c -> c == ';') 
  return (words vs)

edfP :: Parser Edf
edfP = Edf
       <$> edf'LambdaP
       <*> edf'MotorPosP
       <*> edf'MotorMneP
         <?> "edfP"

edfFromFile :: FilePath -> IO Edf
edfFromFile filename = do
  content <- readFile filename
  let header = head (split '}' content)
  return $ case parseOnly edfP (decodeUtf8 header) of
    Left _     -> error $ "Can not parse the " ++ filename ++ " edf file"
    Right a -> a

main :: IO ()
main = do
  edf <- edfFromFile "/home/picca/test.edf"
  print edf
  return ()
