module Day02
  (isSafe, parseReportString, countSafeReports) where

import Data.List (words, sort)

exampleReports :: [[Integer]]
exampleReports = [[7, 6, 4, 2, 1],
                  [1, 2, 7, 8, 9],
                  [9, 7, 6, 2, 1],
                  [1, 3, 2, 4, 5],
                  [8, 6, 4, 4, 1],
                  [1, 3, 6, 7, 9]]

-- Safety conditions:
--   The levels are either all increasing or all decreasing.
--   Any two adjacent levels differ by at least one and at most three.
--     to do this, I can zip report with a shifted report,
--     then map (-) to get differences

isIncreasing :: [Integer] -> Bool
isIncreasing report =
  sort report == report

isDecreasing :: [Integer] -> Bool
isDecreasing report =
  (reverse . sort) report == report

diffIsBounded :: [Integer] -> Bool
diffIsBounded report =
  all isBounded diffs
  where isBounded = (\x -> x >= 1 && x <= 3)
        pairDiff = \(x0,x1) -> abs (x1-x0)
        pairs = zip (tail report) report
        diffs = map pairDiff pairs

isSafe :: [Integer] -> Bool
isSafe report =
  ((isIncreasing report) || (isDecreasing report)) &&
  (diffIsBounded report)

countSafeReports :: [[Integer]] -> Int
countSafeReports reports =
  length $ filter isSafe reports

parseReportString :: String -> [Integer]
parseReportString reportString =
  map read (words reportString)
