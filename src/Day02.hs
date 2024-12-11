module Day02
  (isSafe,
   isSafeWithDampener,
   parseReportString,
   countSafeReports,
   countSafeReportsWithDampener,
   exampleReports) where

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

-- Part 2 has a modification that checks if a single
-- unsafe level is the reason a report is marked unsafe.
--   given a report with n items, we generate a list of n
--   lists with (n-1) items, and then
--      if any of them are safe, return true`

removeNth :: Int -> [a] -> [a]
removeNth _ [] = []
removeNth 0 (_:xs) = xs
removeNth n (x:xs) = x : removeNth (n-1) xs

isSafeWithDampener :: [Integer] -> Bool
isSafeWithDampener r =
  any isSafe [removeNth i r | i <- [0..(length r)-1]]

countSafeReportsWithDampener :: [[Integer]] -> Int
countSafeReportsWithDampener reports =
  length $ filter isSafeWithDampener reports
