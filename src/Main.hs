module Main (main) where

import TSVReader
import Day01
import Day02
import Day03

day01 :: IO [String]
day01 = readTSVFile "./data/day01.tsv"

day02 :: IO [String]
day02 = readTSVFile "./data/day02.tsv"

day03 :: IO [String]
day03 = readTSVFile "./data/day03.txt"

d3mulStrs :: IO [[String]]
d3mulStrs = (fmap . fmap) extractMuls day03
d3mulPairs :: IO [[(Integer,Integer)]]
d3mulPairs = fmap (map (map (parseMul))) d3mulStrs

-- need to (fmap . fmap) parsePair through
-- both the IO and [] functors. 
pairs :: IO [(Integer, Integer)]
pairs = (fmap . fmap) parsePair day01

reports :: IO [[Integer]]
reports = (fmap . fmap) parseReportString day02

main :: IO ()
main = do
  putStrLn "Advent of Code 2024"
  d1ans <- totalDist <$> pairs
  print d1ans
  d1ans' <- simScore <$> pairs
  print d1ans'

  d2ans <- countSafeReports <$> reports
  print d2ans
  d2ans' <- countSafeReportsWithDampener <$> reports
  print d2ans'

  -- I figured this out through trial & error at the GHCi REPL
  d3ans <- sum <$> ((map sum) <$> ((map (map mulPair)) <$> (d3mulPairs)))
  print d3ans
