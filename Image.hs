{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Image where

import Codec.Picture
import Codec.Picture.Saving
import qualified Codec.Picture.Types as M
import Control.Monad.ST
import Data.Either (rights)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import System.Directory (createDirectoryIfMissing)

import Type

photoPath :: Album -> Photo -> String
photoPath album photo = "./photos/" ++ (albumName album) ++ "/" ++ (photoName photo)

albumThumbDir :: Album -> String
albumThumbDir album = "./thumbnails/" ++ (albumName album)

thumbnailPath :: Album -> Photo -> String
thumbnailPath album photo = "./thumbnails/" ++ (albumName album) ++ "/" ++ (photoName photo) ++ ".png"

unwrapAlbums :: (Album -> Photo -> a) -> [Album] -> [a]
unwrapAlbums f albums = concatMap (\a -> map (f a) (photos a)) albums

-- | Scale an image using bi-linear interpolation.
scaleBilinear ::
  -- | Desired width
  Int ->
  -- | Desired height
  Int ->
  -- | Original image
  Image M.PixelRGBA8 ->
  -- | Scaled image
  Image M.PixelRGBA8
scaleBilinear width height img@Image {..}
  | width <= 0 || height <= 0 =
    generateImage (error "scaleBilinear: absurd") (max 0 width) (max 0 height)
  | otherwise = runST $ do
    mimg <- M.newMutableImage width height
    let sx, sy :: Float
        sx = fromIntegral imageWidth / fromIntegral width
        sy = fromIntegral imageHeight / fromIntegral height
        go x' y'
          | x' >= width = go 0 (y' + 1)
          | y' >= height = M.unsafeFreezeImage mimg
          | otherwise = do
            let xf = fromIntegral x' * sx
                yf = fromIntegral y' * sy
                x, y :: Int
                x = floor xf
                y = floor yf
                δx = xf - fromIntegral x
                δy = yf - fromIntegral y
                pixelAt' i j =
                  pixelAt img (min (pred imageWidth) i) (min (pred imageHeight) j)
            writePixel mimg x' y' $
              mulp (pixelAt' x y) ((1 - δx) * (1 - δy))
                `addp` mulp (pixelAt' (x + 1) y) (δx * (1 - δy))
                `addp` mulp (pixelAt' x (y + 1)) ((1 - δx) * δy)
                `addp` mulp (pixelAt' (x + 1) (y + 1)) (δx * δy)
            go (x' + 1) y'
    go 0 0

mulp :: (Pixel a, Integral (PixelBaseComponent a)) => a -> Float -> a
mulp pixel x = colorMap (floor . (* x) . fromIntegral) pixel
{-# INLINE mulp #-}


addp ::
  forall a.
  ( Pixel a,
    Bounded (PixelBaseComponent a),
    Integral (PixelBaseComponent a)
  ) =>
  a ->
  a ->
  a
addp = mixWith (const f)
  where
    f x y =
      fromIntegral $
        (maxBound :: PixelBaseComponent a) `min` (fromIntegral x + fromIntegral y)
{-# INLINE addp #-}

divisor :: Int -> Int
divisor i = if d < 1 then 1 else d
  where d = floor $ (fromIntegral i) / 512.0

scaleImage :: Image M.PixelRGBA8 -> Image M.PixelRGBA8
scaleImage i = scaleBilinear w h i
  where
    d = fromIntegral $ divisor $ min (imageWidth i) (imageHeight i)
    w = floor $ (fromIntegral (imageWidth i)) / d
    h = floor $ (fromIntegral (imageHeight i)) / d

saveImage :: (Image M.PixelRGBA8, String) -> IO ()
saveImage (image, path) = (B.writeFile path . imageToPng . ImageRGBA8 . scaleImage) $ image

saveThumbnails :: [Album] -> IO ()
saveThumbnails albums = do
  imagesE <- mapM readImage $ albumPaths
  let images = fmap convertRGBA8 . rights $ imagesE
  mapM_ (createDirectoryIfMissing True . albumThumbDir) albums
  mapM_ saveImage $ zip images thumbnailPaths

  where
    albumPaths = unwrapAlbums photoPath albums
    thumbnailPaths = unwrapAlbums thumbnailPath albums
    d i = divisor (imageWidth i)
