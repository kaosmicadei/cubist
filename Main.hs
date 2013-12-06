module Main where


import System.Environment

import LoadPNG
import Polygon
import Annealing


main :: IO ()
main = do
  [imagePath, numPolygons, numVertices] <- getArgs

  (goal, size) <- loadPNG imagePath

  simulateAnnealing goal size (read numPolygons) (read numVertices)