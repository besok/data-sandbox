module Interpolation where

import Data.List

linearInterpolation :: (RealFrac a) => Int -> [a] -> a
linearInterpolation p xs
  | null xs = 0
  | p < 0 || p > 100 = 0
  | p == 0 = head sortedXs
  | p == 100 = last sortedXs
  | otherwise =
      let lowerValue = sortedXs !! lowerIndex
          upperValue = sortedXs !! upperIndex
          weight = rank - fromIntegral lowerIndex
       in lowerValue + weight * (upperValue - lowerValue)
  where
    sortedXs = sort xs
    n = length xs
    rank = (fromIntegral p / 100) * fromIntegral (n - 1)
    lowerIndex = floor rank
    upperIndex = min (lowerIndex + 1) (n - 1)