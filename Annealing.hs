{-# LANGUAGE RecordWildCards #-}

module Annealing where


import Polygon

import Graphics.Rendering.Cairo

import System.Random
import Control.Monad.IO.Class

import qualified Data.ByteString as ByteString
import Data.ByteString (ByteString)

import qualified Data.Vector as Vector
import Data.Vector ((!), (//))


evolveSample :: (Num a, Ord a) => ByteString -> a -> Sample -> Int -> Surface -> IO ()
evolveSample _ _ _ 1000 _ = return ()
evolveSample _ 1 _ _ _    = return ()

evolveSample goal previousDistance previousSample generation surface = do
  currentSample <- mutateSample 0.1 previousSample

  currentData <- draw currentSample surface >>= imageSurfaceGetData

  let currentDistance = metric currentData goal

  if currentDistance < previousDistance
    then do
      surfaceWriteToPNG surface $ addZero generation ++ show generation ++ "g.png"

      evolveSample goal currentDistance currentSample (generation+1) surface

    else
      evolveSample goal previousDistance previousSample (generation+1) surface

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