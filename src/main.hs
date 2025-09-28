import AStar (astar)
import Data.List (intercalate)
import GeneticAlgorithm (GAConfig (initialGen), defaultGAConfig, fitness, runGeneticAlgorithm)
import Maze (endNode, manhattan, maze, printPath, startNode)
import Salesman (Route, cities)
import System.Random (mkStdGen)

main :: IO ()
main = do
  -- putStrLn "Running genetic algorithm to find the shortest route..."
  -- -- We need to provide a type signature to tell the compiler which 'Genetic' instance to use.
  -- let config = defaultGAConfig {initialGen = mkStdGen 101}
  -- bestRoute <- runGeneticAlgorithm config :: IO Route
  -- let routeFitness = fitness bestRoute
  -- let totalDistance = 1 / routeFitness
  -- let cityNames = map (cities !!) bestRoute
  -- putStrLn $ "Best route found: " ++ intercalate " -> " cityNames
  -- putStrLn $ "Total distance: " ++ show (round totalDistance :: Int)
  printPath $ astar maze startNode endNode manhattan