module Main where


import System.Environment
import Data.Time
import System.Locale

import qualified Data.ByteString as ByteString
import Data.ByteString (ByteString)

import qualified Graphics.Rendering.Cairo as Cairo
import Graphics.Rendering.Cairo (Format (FormatARGB32))

import LoadPNG
import Polygon


main :: IO ()
main = do
  [imagePath, numPolygons, numVertices] <- getArgs

  (goal, width, height) <- loadPNG imagePath

  polygon <- newPolygon (read numVertices :: Int) (width, height)

  surface <- Cairo.createImageSurface FormatARGB32 width height
  draw [polygon] surface

  currentTime <- getZonedTime

  Cairo.surfaceWriteToPNG surface $ formatTime defaultTimeLocale "%Y-%m-%d_%H-%M-%S.png" currentTime

  polygonM <- mutatePolygon 0.25 polygon

  draw [polygonM] surface

  currentTime <- getZonedTime

  Cairo.surfaceWriteToPNG surface $ formatTime defaultTimeLocale "%Y-%m-%d_%H-%M-%SM.png" currentTime

  Cairo.surfaceFinish surface
