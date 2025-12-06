{-# LANGUAGE TemplateHaskell #-}

import Mayn
import Parsing
import Utils

$(generateMain "02")

type Input = [Coord]

aocParse :: Parser Input
aocParse = ((,) <$> nat <* lexeme "-" <*> nat) `sepBy` lexeme "," <* eof

partOne :: Input -> Int
partOne = sum . map (sum . map mapper . toRange)
  where 
  mapper :: Int -> Int 
  mapper = phoenix (*) id (fromEnum . phoenix (==) fst snd . halve . show)

partTwo :: Input -> Int
partTwo = sum . map snd
