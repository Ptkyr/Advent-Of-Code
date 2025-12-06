{-# LANGUAGE TemplateHaskell #-}

import Mayn
import Parsing
import Utils

$(generateMain "02")

type Input = [Coord]

aocParse :: Parser Input
aocParse = ((,) <$> nat <* lexeme "-" <*> nat) `sepBy` lexeme "," <* eof

partOne :: Input -> Int
partOne = sum . concatMap (map mapper . toRange)
  where 
  mapper :: Int -> Int 
  mapper = (*) <*> fromEnum . uncurry (==) . halve . show

partTwo :: Input -> Int
partTwo = sum . concatMap (map mapper . toRange)
  where
  mapper :: Int -> Int
  mapper = (*) <*> fromEnum . invalid . show
    where
    invalid :: String -> Bool
    invalid str = any (flip (allEqual .: chunksOf) str) [1 .. (length str `div` 2)]
