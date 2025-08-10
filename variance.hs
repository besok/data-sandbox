module Variance where

import Mean

variance :: (Fractional a) => [a] -> a
variance [] = 0
variance xs = sumXs / len
  where
    meanXs = mean xs
    len = fromIntegral (length xs)
    sumXs = sum [(x - meanXs) ^ 2 | x <- xs]

testVariance :: (Fractional a, Eq a, Show a) => String -> [a] -> a -> String
testVariance name xs expected = name ++ ": " ++ result
  where
    actual = variance xs
    result =
      if actual == expected
        then "Passed"
        else "Failed, expected " ++ show expected ++ " but got " ++ show actual