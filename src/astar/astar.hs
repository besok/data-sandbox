module AStar where

import Maze (MazeStruct, Path, Point)

type Heuristic = Point -> Point -> Int

type StartPoint = Point

type EndPoint = Point

astar :: MazeStruct -> StartPoint -> EndPoint -> Heuristic -> Path
astar maze start end heuristic = [(0, 0), (1, 1), (2, 2)]