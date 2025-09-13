import System.Random (StdGen)

module GeneticAlgorithm where

type Gen a = StdGen -> (a, StdGen)

class Genetic ch where
    -- | Creates a single, random individual (chromosome).
    initial :: Gen ch
    
    -- | Combines two parents to create a child.
    crossover :: ch -> ch -> Gen ch
    
    -- | Applies a small, random change to an individual.
    mutate :: ch -> Gen ch

    -- | Calculates the fitness of an individual. Higher is better.
    fitness :: ch -> Double


-- | Configuration parameters for the genetic algorithm.
data GAConfig = GAConfig
  { populationSize :: Int      -- ^ The number of individuals in each generation.
  , maxGenerations :: Int      -- ^ The maximum number of generations to run.
  , mutationRate   :: Double   -- ^ The probability (0.0 to 1.0) that a gene will mutate.
  , crossoverRate  :: Double   -- ^ The probability (0.0 to 1.0) that parents will create offspring.
  , elitismCount   :: Int      -- ^ The number of best individuals to carry over to the next generation.
  , initialGen     :: StdGen   -- ^ The initial random number generator for reproducibility.
  } deriving (Show)

runGeneticAlgorithm :: Genetic ch, Ord ch => GAConfig -> IO ch