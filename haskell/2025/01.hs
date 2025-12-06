{-# LANGUAGE TemplateHaskell #-}

import Mayn
import Parsing
import Utils

$(generateMain "01")

type Input = [Int]

aocParse :: Parser Input
aocParse =
  some
    ( choice
        [ negate <$> (char 'L' *> nat),
          char 'R' *> nat
        ]
    )
    <* eof

partOne :: Input -> Int
partOne = length . filter (== 0) . scanl' (flip mod 100 .: (+)) 50

partTwo :: Input -> Int
partTwo = snd . foldl' mapper (50, 0)
  where 
    mapper :: (Int, Int) -> Int -> (Int, Int)
    mapper (dial, cnt) offset = (newDial, cnt + rotations + fromEnum special)
      where 
        rawDial = dial + offset
        newDial = rawDial `mod` 100
        rotations = abs rawDial `div` 100
        special = offset < 0 && dial /= 0 && rawDial <= 0
