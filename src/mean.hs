module Mean where

mean :: (Fractional a) => [a] -> a
mean [] = 0
mean xs = sum xs / fromIntegral (length xs)

testMean :: (Fractional a, Eq a, Show a) => String -> [a] -> a -> String
testMean name xs expected = name ++ ": " ++ result
  where
    actual = mean xs
    result =
      if actual == expected
        then "Passed"
        else "Failed, expected " ++ show expected ++ " but got " ++ show actual