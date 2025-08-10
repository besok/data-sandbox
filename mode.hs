module Mode where

import qualified Data.Map as Map

mode :: (Eq a, Ord a) => [a] -> Int
mode [] = 0
mode xs = maximum (Map.elems frequencies)
  where
    frequencies = Map.fromListWith (+) [(x, 1) | x <- xs]

testMode :: (Eq a, Ord a, Show a) => String -> [a] -> Int -> String
testMode name xs expected = name ++ ": " ++ result
  where
    actual = mode xs
    result =
      if actual == expected
        then "Passed"
        else "Failed, expected " ++ show expected ++ " but got " ++ show actual