module Day01
  (listPairs, totalDist) where

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
  dists = map (\(l1,l2) -> l2-l1) (zip list1 list2)
    where list1 = sort $ map fst pairs
          list2 = sort $ map snd pairs
