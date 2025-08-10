module Median where

import Data.List

median :: (Ord a, Integral a) => [a] -> a
median [] = 0
median xs
  | odd len = sorted !! mid
  | otherwise = (sorted !! mid + sorted !! (mid - 1)) `div` 2
  where
    len = length xs
    sorted = sort xs
    mid = len `div` 2

testMedian :: String -> [Integer] -> Integer -> String
testMedian name xs expected = name ++ ": " ++ result
  where
    actual = median xs
    result =
      if actual == expected
        then "Passed"
        else "Failed, expected " ++ show expected ++ " but got " ++ show actual
