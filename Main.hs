module Main where


import System.Environment

import LoadPNG
import qualified Data.ByteString as ByteString

import Polygon


main :: IO ()
main = do
  [imagePath] <- getArgs

  (goal, width, height) <- loadPNG imagePath

  print width
  print height
  print $ length (ByteString.unpack goal)

  polygon <- newPolygon 5 (width, height)

  draw polygon (width, height)
