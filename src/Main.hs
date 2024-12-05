module Main (main) where

import TSVReader
import Day01

contents :: IO [String]
contents = readTSVFile "./data/day01.tsv"

-- need to (fmap . fmap) parsePair through
-- both the IO and [] functors. 
pairs :: IO [(Integer, Integer)]
pairs = (fmap . fmap) parsePair contents

main :: IO ()
main = do
  putStrLn "Advent of Code 2024"
  d1ans <- totalDist <$> pairs
  print d1ans

