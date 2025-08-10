import Mean
import Median
import Mode
import Variance

main = do
  putStrLn $ testMedian "Test with odd" [5, 3, 7, 8, 5] 5
  putStrLn $ testMedian "Test with even" [1, 2, 3, 4] 2
  putStrLn $ testMedian "Test with single" [10] 10
  putStrLn $ testMedian "Test with empty" [] 0
  putStrLn $ testMean "Mean with integers" [1, 2, 3, 4] 2
  putStrLn $ testMean "Mean with floats" [1.5, 2.5, 3.5] 2
  putStrLn $ testMean "Mean with empty" [] 0

  putStrLn $ testMode "Mode with integers" [1, 2, 2, 3] 2
  putStrLn $ testMode "Mode with single" [5] 1

  putStrLn $ testVariance "Variance with integers" [1, 2, 3, 4] 1.25
  putStrLn $ testVariance "Variance with floats" [1.5, 2.5, 3.5] 0.6666666666666666
  putStrLn $ testVariance "Variance with empty" [] 0