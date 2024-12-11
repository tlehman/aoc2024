module Day03 (muls, mulPair, parseMul) where

import Data.List.Split (splitOneOf)
import Text.Regex.PCRE

corruptedMemory :: String
corruptedMemory = "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"


mulsResult :: AllTextMatches [] String
mulsResult = corruptedMemory =~ "mul\\(\\d+,\\d+\\)"
muls :: [String]
muls = getAllTextMatches mulsResult

mulPair :: (Integer, Integer) -> Integer
mulPair (a,b) = a*b

-- parse "mul(a,b)" to just (a,b)
parseMul :: String -> (Integer, Integer)
parseMul mulStr =
  let split = splitOneOf "(,)" mulStr
  in case split of
    [_, a, b, _] -> (read a, read b)
    _ -> (-1, -1)
