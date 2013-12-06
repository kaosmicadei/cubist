module Main where


import System.Environment
import Data.Time
import System.Locale


import qualified Data.ByteString as ByteString
import Data.ByteString (ByteString)

import Graphics.Rendering.Cairo

import LoadPNG
import Polygon
import Annealing


main :: IO ()
main = do
  [imagePath, numPolygons, numVertices] <- getArgs

  (goal, width, height) <- loadPNG imagePath

  withImageSurface FormatARGB32 width height $ \surface -> do
    initialSample <- newSample (read numVertices) (width, height) (read numPolygons)

    initialData <- draw initialSample surface >>= imageSurfaceGetData

    let initialDistance = metric initialData goal

    surfaceWriteToPNG surface "00g.png"

    evolveSample goal initialDistance initialSample 1 surface