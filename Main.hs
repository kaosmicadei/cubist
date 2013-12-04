module Main where


import System.Environment
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

  polygons <- listOfPolygons (read numVertices :: Int) (width, height) (read numPolygons :: Int)

  surface <- Cairo.createImageSurface FormatARGB32 width height
  result <- draw polygons surface >>= Cairo.imageSurfaceGetData

  Cairo.surfaceWriteToPNG surface "polygons.png"

  Cairo.surfaceFinish surface
  
  print $ fitness result goal

 where
  fitness :: ByteString -> ByteString -> Double
  fitness bs1 bs2 = 1 - diff / fromIntegral (255 * ByteString.length bs2)
   where
    diff = sum . map fromIntegral $ ByteString.zipWith metric bs1 bs2
    metric a b = abs (a - b)
