{-# LANGUAGE RecordWildCards #-}

module Polygon where


import System.Random
import Control.Monad (forM_)

import qualified Graphics.Rendering.Cairo as Cairo
import Graphics.Rendering.Cairo (Surface)


type Size = (Int, Int)
type Position = (Double, Double)


data Polygon = Polygon { red      :: Double
                       , green    :: Double
                       , blue     :: Double
                       , alpha    :: Double
                       , vertices :: [Position]
                       }
  deriving (Show)


newPolygon :: Int -> Size -> IO Polygon
newPolygon numVertices (width, height) = do
  g <- newStdGen

  let [red, green, blue, alpha] = take 4 (randomRs (0.0, 1.0) g)

  gx <- newStdGen
  gy <- newStdGen

  let xs = randomRs (0, fromIntegral width) gx
      ys = randomRs (0, fromIntegral height) gy
      vertices = take numVertices (zip xs ys)

  return (Polygon red green blue alpha vertices)


listOfPolygons :: Int -> Size -> Int -> IO [Polygon]
listOfPolygons numVertices size numPolygons =
  sequence $ replicate numPolygons (newPolygon numVertices size)


draw :: [Polygon] -> Surface -> IO Surface
draw polygons surface =
  Cairo.renderWith surface $ do
    width  <- fmap fromIntegral $ Cairo.imageSurfaceGetWidth  surface
    height <- fmap fromIntegral $ Cairo.imageSurfaceGetHeight surface 

    Cairo.setSourceRGB 1 1 1
    Cairo.rectangle 0 0 width height
    Cairo.fill

    forM_ polygons $ \Polygon {..} -> do
      Cairo.setSourceRGBA red green blue alpha
      Cairo.setLineWidth 0

      let (startX, startY) = head vertices

      Cairo.moveTo startX startY

      forM_ vertices $ \(x,y) ->
        Cairo.lineTo x y

      Cairo.fill

    return surface
