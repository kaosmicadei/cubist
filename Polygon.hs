{-# LANGUAGE RecordWildCards #-}

module Polygon where


import System.Random
import Control.Monad (forM_)
import Control.Monad.IO.Class

import qualified Data.Vector as Vector
import Data.Vector (Vector)

import qualified Graphics.Rendering.Cairo as Cairo
import Graphics.Rendering.Cairo (Surface)


type Size = (Int, Int)
type Position = (Double, Double)
type Sample = Vector Polygon


data Polygon = Polygon { red         :: !Double
                       , green       :: !Double
                       , blue        :: !Double
                       , alpha       :: !Double
                       , size        :: !Size
                       , numVertices :: !Int
                       , vertices    :: [Position]
                       } deriving (Show)


newPolygon :: MonadIO m => Int -> Size -> m Polygon
newPolygon numVertices (width, height) = do
  g <- liftIO newStdGen

  let [red, green, blue, alpha] = take 4 $ randomRs (0, 1 :: Double) g

  gx <- liftIO newStdGen
  gy <- liftIO newStdGen

  let xs = randomRs (0, fromIntegral width) gx
      ys = randomRs (0, fromIntegral height) gy
      vertices = take numVertices (zip xs ys)

  return (Polygon red green blue alpha (width, height) numVertices vertices)


newSample :: MonadIO m => Int -> Size -> Int -> m Sample
newSample numVertices size numPolygons =
  Vector.replicateM numPolygons (newPolygon numVertices size)

  
mutatePolygon :: MonadIO m => Double -> Polygon -> m Polygon
mutatePolygon delta p@Polygon {..} = do
  gen <- liftIO newStdGen

  chance <- liftIO $ randomRIO (0, 1 :: Double)

  if chance < 0.5
    then do
      let variations = take 4 $ shift delta gen
          [newRed, newGreen, newBlue, newAlpha] = map (normalize 1) $ zipWith (+) variations [red,green,blue,alpha]

      return p { red   = newRed
               , green = newGreen
               , blue  = newBlue
               , alpha = newAlpha
               }

    else do
      gen1 <- liftIO newStdGen

      let width  = fromIntegral (fst size)
          height = fromIntegral (snd size)
          variationsX  = take numVertices $ shift (width * delta)  gen
          variationsY  = take numVertices $ shift (height * delta) gen1
          newVertices  = map (normalizePosition size) $ zipWith move vertices (zip variationsX variationsY) 
          
      return p { vertices = newVertices }
      
 where
  shift d g = randomRs (-d, d) g

  normalize top = max 0 . min top

  normalizePosition (w,h) (x,y) = (normalize w' x, normalize h' y)
   where
    w' = fromIntegral w
    h' = fromIntegral h

  move (x,y) (d1,d2) = (x + d1, y+ d2)


draw :: MonadIO m => Sample -> Surface -> m Surface
draw polygons surface =
  Cairo.renderWith surface $ do
    width  <- fmap fromIntegral $ Cairo.imageSurfaceGetWidth  surface
    height <- fmap fromIntegral $ Cairo.imageSurfaceGetHeight surface 

    Cairo.setSourceRGB 1 1 1
    Cairo.rectangle 0 0 width height
    Cairo.fill

    forM_ (Vector.toList polygons) $ \Polygon {..} -> do
      Cairo.setSourceRGBA red green blue alpha
      Cairo.setLineWidth 0

      let (startX, startY) = head vertices

      Cairo.moveTo startX startY

      forM_ vertices $ \(x,y) ->
        Cairo.lineTo x y

      Cairo.fill

    return surface
