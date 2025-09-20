{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Salesman where

import Data.Array
import GeneticAlgorithm
import System.Random (StdGen, mkStdGen, randomR)

type CityId = Int

type Distance = Int

type Route = [CityId]

distances :: Array (CityId, CityId) Distance
distances =
  array
    ((0, 0), (9, 9))
    [ ((0, 0), 0),
      ((0, 1), 1054),
      ((0, 2), 1508),
      ((0, 3), 2318),
      ((0, 4), 1100),
      ((0, 5), 680),
      ((0, 6), 575),
      ((0, 7), 1330),
      ((0, 8), 650),
      ((0, 9), 1030), -- Berlin
      ((1, 0), 1054),
      ((1, 1), 0),
      ((1, 2), 1420),
      ((1, 3), 1270),
      ((1, 4), 460),
      ((1, 5), 1235),
      ((1, 6), 1600),
      ((1, 7), 2450),
      ((1, 8), 505),
      ((1, 9), 1850), -- Paris
      ((2, 0), 1508),
      ((2, 1), 1420),
      ((2, 2), 0),
      ((2, 3), 1960),
      ((2, 4), 1880),
      ((2, 5), 1100),
      ((2, 6), 1800),
      ((2, 7), 2320),
      ((2, 8), 1660),
      ((2, 9), 2450), -- Rome
      ((3, 0), 2318),
      ((3, 1), 1270),
      ((3, 2), 1960),
      ((3, 3), 0),
      ((3, 4), 1730),
      ((3, 5), 2500),
      ((3, 6), 2870),
      ((3, 7), 3720),
      ((3, 8), 1760),
      ((3, 9), 3120), -- Madrid
      ((4, 0), 1100),
      ((4, 1), 460),
      ((4, 2), 1880),
      ((4, 3), 1730),
      ((4, 4), 0),
      ((4, 5), 1600),
      ((4, 6), 1780),
      ((4, 7), 2600),
      ((4, 8), 530),
      ((4, 9), 1820), -- London
      ((5, 0), 680),
      ((5, 1), 1235),
      ((5, 2), 1100),
      ((5, 3), 2500),
      ((5, 4), 1600),
      ((5, 5), 0),
      ((5, 6), 670),
      ((5, 7), 1400),
      ((5, 8), 1140),
      ((5, 9), 1450), -- Vienna
      ((6, 0), 575),
      ((6, 1), 1600),
      ((6, 2), 1800),
      ((6, 3), 2870),
      ((6, 4), 1780),
      ((6, 5), 670),
      ((6, 6), 0),
      ((6, 7), 760),
      ((6, 8), 1200),
      ((6, 9), 1150), -- Warsaw
      ((7, 0), 1330),
      ((7, 1), 2450),
      ((7, 2), 2320),
      ((7, 3), 3720),
      ((7, 4), 2600),
      ((7, 5), 1400),
      ((7, 6), 760),
      ((7, 7), 0),
      ((7, 8), 2050),
      ((7, 9), 1700), -- Kyiv
      ((8, 0), 650),
      ((8, 1), 505),
      ((8, 2), 1660),
      ((8, 3), 1760),
      ((8, 4), 530),
      ((8, 5), 1140),
      ((8, 6), 1200),
      ((8, 7), 2050),
      ((8, 8), 0),
      ((8, 9), 1430), -- Amsterdam
      ((9, 0), 1030),
      ((9, 1), 1850),
      ((9, 2), 2450),
      ((9, 3), 3120),
      ((9, 4), 1820),
      ((9, 5), 1450),
      ((9, 6), 1150),
      ((9, 7), 1700),
      ((9, 8), 1430),
      ((9, 9), 0) -- Stockholm
    ]

cities :: [String]
cities =
  [ "Berlin", -- 0
    "Paris", -- 1
    "Rome", -- 2
    "Madrid", -- 3
    "London", -- 4
    "Vienna", -- 5
    "Warsaw", -- 6
    "Kyiv", -- 7
    "Amsterdam", -- 8
    "Stockholm" -- 9
  ]

-- Helper function to swap two elements in a list
swap :: Int -> Int -> [a] -> [a]
swap i j xs
  | i == j = xs
  | i > j = swap j i xs
  | otherwise =
      let x_i = xs !! i
          x_j = xs !! j
          (before, rest) = splitAt i xs
          (middle, after) = splitAt (j - i - 1) (drop (i + 1) rest)
       in before ++ [x_j] ++ middle ++ [x_i] ++ drop 1 after

instance Genetic Route where
  initial gen =
    let (route, gen') = shuffle [0 .. length cities - 1] gen
     in (route, gen')
    where
      shuffle [] g = ([], g)
      shuffle xs g =
        let (i, g1) = randomR (0, length xs - 1) g
            x = xs !! i
            xs' = take i xs ++ drop (i + 1) xs
            (rest, g2) = shuffle xs' g1
         in (x : rest, g2)

  crossover parent1 parent2 gen =
    let len = length parent1
        (start, gen1) = randomR (0, len - 1) gen
        (end, gen2) = randomR (start, len - 1) gen1
        slice = take (end - start + 1) . drop start $ parent1
        remaining = filter (`notElem` slice) parent2
        child = take start remaining ++ slice ++ take (len - end - 1) (drop start remaining)
     in (child, gen2)

  mutate route gen =
    let len = length route
        (i, gen1) = randomR (0, len - 1) gen
        (j', gen2) = randomR (0, len - 2) gen1
        -- Ensure j is not equal to i
        j = if j' < i then j' else j' + 1
        route' = swap i j route
     in (route', gen2)

  fitness route =
    1.0 / fromIntegral (totalDistance route)
    where
      totalDistance :: Route -> Distance
      totalDistance r = sum $ zipWith (curry (distances !)) r (tail r ++ [head r])