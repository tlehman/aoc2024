module Day01
  (listPairs, totalDist, parsePair, parsePairs) where

import Data.List (sort)

listPairs :: [(Integer, Integer)]
listPairs = [(3, 4),
             (4, 3),
             (2, 5),
             (1, 3),
             (3, 9),
             (3, 3)]
  
totalDist :: [(Integer, Integer)] -> Integer
totalDist pairs = sum dists where
  dists = map (\(l1,l2) -> abs(l2-l1)) (zip list1 list2)
    where list1 = sort $ map fst pairs
          list2 = sort $ map snd pairs

-- Parsing data for Day 1:
--   words "77221   93653" == ["77221","93653"]
--   map (\s -> read s :: Integer) ["77221", "93653"] == [77221,93653]

parsePair :: String -> (Integer, Integer)
parsePair str =
  let [a, b] = map (\s -> read s :: Integer) (words str)
  in (a, b)

parsePairs :: [String] -> [(Integer, Integer)]
parsePairs strs = map parsePair strs
