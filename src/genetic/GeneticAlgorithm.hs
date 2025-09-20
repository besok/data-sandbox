{-# LANGUAGE ScopedTypeVariables #-}

module GeneticAlgorithm where

import Data.List (sortBy)
import System.Random

type Gen a = StdGen -> (a, StdGen)

class Genetic chromosome where
  initial :: Gen chromosome
  crossover :: chromosome -> chromosome -> Gen chromosome
  mutate :: chromosome -> Gen chromosome
  fitness :: chromosome -> Double

data GAConfig = GAConfig
  { populationSize :: Int,
    maxGenerations :: Int,
    mutationRate :: Double,
    crossoverRate :: Double,
    elitismCount :: Int,
    initialGen :: StdGen
  }
  deriving (Show)

defaultGAConfig =
  GAConfig
    { populationSize = 1000,
      maxGenerations = 10000,
      mutationRate = 0.01,
      crossoverRate = 0.7,
      elitismCount = 2,
      initialGen = mkStdGen 42
    }

runGen :: StdGen -> Gen a -> (a, StdGen)
runGen gen f = f gen

select :: (Genetic ch) => StdGen -> [ch] -> (ch, StdGen)
select gen population =
  let (i1, gen') = randomR (0, length population - 1) gen
      (i2, gen'') = randomR (0, length population - 1) gen'
      ch1 = population !! i1
      ch2 = population !! i2
   in if fitness ch1 > fitness ch2 then (ch1, gen'') else (ch2, gen'')

runGeneticAlgorithm :: (Genetic ch) => GAConfig -> IO ch
runGeneticAlgorithm config = do
  let evolve :: forall ch. (Genetic ch) => Int -> StdGen -> [ch] -> IO ch
      evolve generationCount gen population = do
        if generationCount >= maxGenerations config
          then do
            -- Sort and return the best chromosome
            let sortedPopulation = sortBy (\a b -> compare (fitness b) (fitness a)) population
            return $ head sortedPopulation
          else do
            let sortedPopulation = sortBy (\a b -> compare (fitness b) (fitness a)) population
                elite = take (elitismCount config) sortedPopulation

            let generateNewPopulation :: StdGen -> Int -> [ch] -> ([ch], StdGen)
                generateNewPopulation currentGen n newPopulation =
                  if n >= populationSize config - (elitismCount config)
                    then (reverse newPopulation, currentGen)
                    else
                      let -- Selection
                          (parent1, gen1) = select currentGen sortedPopulation
                          (parent2, gen2) = select gen1 sortedPopulation

                          -- Crossover
                          (doCrossover, gen3) = randomR (0.0, 1.0) gen2

                          (child, gen4) =
                            if doCrossover < crossoverRate config
                              then runGen gen3 (crossover parent1 parent2)
                              else (parent1, gen3) -- Fallback

                          -- Mutation
                          (doMutate, gen5) = randomR (0.0, 1.0) gen4

                          (mutatedChild, gen6) =
                            if doMutate < mutationRate config
                              then runGen gen5 (mutate child)
                              else (child, gen5)
                       in generateNewPopulation gen6 (n + 1) (mutatedChild : newPopulation)

            let (newIndividuals, nextGen) = generateNewPopulation gen 0 []
                nextPopulation = elite ++ newIndividuals

            -- Recursive call for the next generation
            evolve (generationCount + 1) nextGen nextPopulation

  let initialPopulation =
        let generatePop n gen list =
              if n <= 0
                then (list, gen)
                else
                  let (ch, newGen) = runGen gen initial
                   in generatePop (n - 1) newGen (ch : list)
         in fst (generatePop (populationSize config) (initialGen config) [])

  evolve 0 (initialGen config) initialPopulation