import Control.Monad (when)
import Data.List (elemIndex)
import GHC.Real (fromIntegral)
import Interpolation
import Mean
import Median
import Mode
import Students
import Temp
import Variance
import Prelude

main = do
  putStrLn $ testMedian "Test with odd" [5, 3, 7, 8, 5] 5
  putStrLn $ testMedian "Test with even" [1, 2, 3, 4] 2
  putStrLn $ testMedian "Test with single" [10] 10
  putStrLn $ testMedian "Test with empty" [] 0
  putStrLn $ testMean "Mean with integers" [1, 2, 3, 4] 2.5
  putStrLn $ testMean "Mean with floats" [1.5, 2.5, 3.5] 2.5
  putStrLn $ testMean "Mean with empty" [] 0

  putStrLn $ testMode "Mode with integers" [1, 2, 2, 3] 2
  putStrLn $ testMode "Mode with single" [5] 1

  putStrLn $ testVariance "Variance with integers" [1, 2, 3, 4] 1.25
  putStrLn $ testVariance "Variance with floats" [1.5, 2.5, 3.5] 0.6666666666666666
  putStrLn $ testVariance "Variance with empty" [] 0

  putStrLn "Exam Results:"
  putStrLn $ " - mean " ++ show (mean $ map (fromIntegral . examScore) exam)
  putStrLn $ " - median " ++ show (median $ map (fromIntegral . examScore) exam)
  putStrLn $ " - mode " ++ show (mode $ map (fromIntegral . examScore) exam)
  putStrLn $ " - variance " ++ show (variance $ map (fromIntegral . examScore) exam)
  putStrLn $ " - 25 percentile " ++ show (linearInterpolation 25 $ map (fromIntegral . examScore) exam)
  putStrLn $ " - 50 percentile " ++ show (linearInterpolation 50 $ map (fromIntegral . examScore) exam)
  putStrLn $ " - 75 percentile " ++ show (linearInterpolation 75 $ map (fromIntegral . examScore) exam)

  putStrLn "Berlin Temperatures 2023-2024:"
  putStrLn $ " - average temp in spring (Mar, Apr, May) " ++ show (mean springTemp)
  putStrLn $ " - average temp in winter (Dec, Jan, Feb) " ++ show (mean winterTemp)
  putStrLn $ " - average temp in summer (Jum, Jul, Aug) " ++ show (mean summerTemp)
  putStrLn $ " - average temp in autumn (Sep, Oct, Nov) " ++ show (mean autumnTemp)
  putStrLn $ " - warmest month " ++ maybe "Undefined" month maxIdx
  putStrLn $ " - coldest month " ++ maybe "Undefined" month minIdx
  
  putStrLn "\nSeasonal Variance and Standard Deviation:"
  putStrLn $ " - Spring variance: " ++ show (variance springTemp) ++ ", std dev: " ++ show (sqrt $ variance springTemp)
  putStrLn $ " - Winter variance: " ++ show (variance winterTemp) ++ ", std dev: " ++ show (sqrt $ variance winterTemp)
  putStrLn $ " - Summer variance: " ++ show (variance summerTemp) ++ ", std dev: " ++ show (sqrt $ variance summerTemp)
  putStrLn $ " - Autumn variance: " ++ show (variance autumnTemp) ++ ", std dev: " ++ show (sqrt $ variance autumnTemp)
  where
    springTemp = map (berlinTemps2023_2024 !!) [2, 3, 4, 14, 15, 16]
    winterTemp = map (berlinTemps2023_2024 !!) [0, 1, 2, 12, 13, 14]
    summerTemp = map (berlinTemps2023_2024 !!) [5, 6, 7, 17, 18, 19]
    autumnTemp = map (berlinTemps2023_2024 !!) [8, 9, 10, 20, 21, 22]
    maxIdx = elemIndex (maximum berlinTemps2023_2024) berlinTemps2023_2024
    minIdx = elemIndex (minimum berlinTemps2023_2024) berlinTemps2023_2024
