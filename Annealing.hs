{-# LANGUAGE RecordWildCards #-}

module Annealing (simulateAnnealing) where


import Polygon

import Graphics.Rendering.Cairo

import System.Random
import System.Posix.Time
import Control.Monad.IO.Class

import qualified Data.ByteString as ByteString
import Data.ByteString (ByteString)

import qualified Data.Vector as Vector
import Data.Vector ((!), (//))


simulateAnnealing :: ByteString -> Size -> Int -> Int -> IO ()
simulateAnnealing goal size@(width, height) numPolygons numVertices =
  withImageSurface FormatARGB32 width height $ \surface -> do
    initialSample <- newSample numVertices size numPolygons

    initialData <- draw initialSample surface >>= imageSurfaceGetData

    let initialDistance = metric initialData goal

    time <- epochTime

    surfaceWriteToPNG surface $ "images/" ++ show time ++ ".png"

    nextSample goal initialDistance initialSample 1 surface


nextSample :: (Fractional a, Ord a) => ByteString -> a -> Sample -> Int -> Surface -> IO ()
nextSample _ _ _ 1800 _ = return ()

nextSample _ d _ _ _ | d < 0.1 = return ()

nextSample goal previousDistance previousSample successes surface = do
  currentSample <- mutateSample 0.1 previousSample

  currentData <- draw currentSample surface >>= imageSurfaceGetData

  let currentDistance = metric currentData goal

  if currentDistance < previousDistance
    then do
      time <- epochTime

      surfaceWriteToPNG surface $ "images/" ++ show time ++ ".png"

      nextSample goal currentDistance currentSample (successes+1) surface

    else
      nextSample goal previousDistance previousSample successes surface

 where
   addZero n = if n < 10 then "0" else "" 

mutateSample :: MonadIO m => Double -> Sample -> m Sample
mutateSample delta sample = do
  let size = Vector.length sample - 1

  position <- liftIO $ randomRIO (0, size)

  mutated <-mutatePolygon delta $ sample!position

  return $ sample // [(position, mutated)]


metric :: Num a => ByteString -> ByteString -> a
metric xs ys = sum $ zipWith (\x y -> abs (x-y)) xs' ys'
 where
  xs' = map fromIntegral $ ByteString.unpack xs
  ys' = map fromIntegral $ ByteString.unpack ys
