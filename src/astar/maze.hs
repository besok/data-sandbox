module Maze where

import qualified Data.Set as Set
import Text.Read (Lexeme (Char))

type X = Int

type Y = Int

type Point = (X, Y)

type Path = [Point]

type MazeStruct = [[Int]]

maze :: MazeStruct
maze =
  [ [0, 0, 0, 0, 1, 0, 0, 0, 0, 0],
    [0, 1, 1, 0, 1, 0, 1, 0, 1, 0],
    [0, 1, 0, 0, 0, 0, 1, 0, 1, 0],
    [0, 0, 0, 1, 1, 1, 1, 0, 0, 0],
    [0, 1, 0, 0, 0, 0, 0, 0, 1, 0],
    [0, 1, 0, 1, 1, 0, 1, 1, 1, 0],
    [0, 1, 0, 0, 1, 0, 0, 0, 1, 0],
    [0, 0, 0, 0, 1, 0, 1, 0, 0, 0],
    [1, 1, 1, 0, 1, 0, 1, 1, 1, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
  ]

startNode :: Point
startNode = (0, 0)

endNode :: Point
endNode = (9, 9)

manhattan :: Point -> Point -> Int
manhattan (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

printPath :: Path -> IO ()
printPath path = do
  let pathSet = Set.fromList path
      height = length maze
      width = length (head maze)
      rows =
        [[cellChar (x, y) | x <- [0 .. width - 1]] | y <- [0 .. height - 1]]
        where
          cellChar p@(x, y)
            | p == startNode = 'S'
            | p == endNode = 'E'
            | p `Set.member` pathSet = '*'
            | (maze !! y !! x) == 1 = '#'
            | otherwise = ' '
  mapM_ putStrLn rows