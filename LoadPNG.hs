module LoadPNG where


import qualified Graphics.Rendering.Cairo as Cairo
import qualified Data.ByteString as ByteString
import Data.ByteString (ByteString)


loadPNG :: FilePath -> IO (ByteString, Int, Int)
loadPNG imagePath = do
  image <- Cairo.imageSurfaceCreateFromPNG imagePath

  width  <- Cairo.imageSurfaceGetWidth image
  height <- Cairo.imageSurfaceGetHeight image

  imgData <- Cairo.imageSurfaceGetData image

  return (imgData, width, height)
