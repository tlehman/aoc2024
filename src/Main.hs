module Main (main) where

import Day01

main :: IO ()
main = do
  putStrLn "Advent of Code 2024"
  print $ totalDist listPairs
