module Day01
  (listPairs, totalDist, parsePair, parsePairs, simScore, frequencyMap) where

import Data.List (foldl', sort)
import qualified Data.HashMap.Strict as HashMap

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

frequencyMap :: [Integer] -> HashMap.HashMap Integer Integer
frequencyMap = foldl' (\acc x -> HashMap.insertWith (+) x 1 acc) HashMap.empty

convertToInteger :: Maybe Integer -> Integer
convertToInteger Nothing = 0
convertToInteger (Just n) = n

-- Similarity score sums up l0*f(l0) + l1*f(l1) + ... ln*f(ln)
-- where l0 is the first number on the left and f(l0) is the frequency
-- of occurrence of l0 in the right list.
simScore :: [(Integer, Integer)] -> Integer
simScore pairs =
  sum [l0 * f(l0) | l0 <- map fst pairs]
  where f x = convertToInteger $ HashMap.lookup x rfm
          where rfm = frequencyMap (map snd pairs)
  

