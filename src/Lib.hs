{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib (render, defaultImageConfig, defaultViewportConfig, defaultRenderConfig) where

import Color (RGB, rgbInt)
import Data.RVar
import Data.Random
import Data.Ratio ((%))
import Hittable
  ( Hit (Hit, hitAt, hitNormal, hitPoint),
    Hittable,
    hit,
  )
import PPM (PPM (PPM))
import Ray (Ray (Ray))
import System.Random
import Vec (R3 (R3), cdiv, ctimes, minus, plus, unit, vmean)

data ImageConfig = ImageConfig
  { imWidth :: Int,
    imHeight :: Int
  }

aspectRatio :: ImageConfig -> Ratio Int
aspectRatio (ImageConfig w h) = w % h

defaultImageConfig :: ImageConfig
defaultImageConfig = ImageConfig {imWidth = 800, imHeight = 450}

data ViewportConfig = ViewportConfig
  { viewportHeight :: Double,
    focalLength :: Double,
    origin :: R3 Double,
    horizontal :: R3 Double,
    vertical :: R3 Double
  }

defaultViewportConfig :: ImageConfig -> ViewportConfig
defaultViewportConfig ic =
  let height = 2.0
   in ViewportConfig
        { viewportHeight = height,
          focalLength = 1.0,
          origin = R3 0 0 0,
          horizontal = R3 (viewportWidth ic (defaultViewportConfig ic)) 0 0,
          vertical = R3 0 height 0
        }

data RenderConfig = RenderConfig
  { numAntialiasingSamples :: Int,
    renderBackground :: Ray -> RGB,
    renderSeed :: Int
  }

defaultRenderConfig :: RenderConfig
defaultRenderConfig =
  RenderConfig
    { numAntialiasingSamples = 30,
      renderBackground = \(Ray _ dir) ->
        let R3 _ y _ = unit dir
            s = 0.5 * (y + 1.0)
         in (R3 1 1 1 `ctimes` (1.0 - s)) `plus` (R3 0.5 0.7 1.0 `ctimes` s),
      renderSeed = 137
    }

viewportWidth :: ImageConfig -> ViewportConfig -> Double
viewportWidth ic ViewportConfig {..} = realToFrac (aspectRatio ic) * viewportHeight

lowerLeftCorner :: ViewportConfig -> R3 Double
lowerLeftCorner ViewportConfig {..} =
  origin `minus` (horizontal `cdiv` 2) `minus` (vertical `cdiv` 2) `minus` R3 0 0 focalLength

rayColor :: Hittable a => a -> (Ray -> RGB) -> Ray -> RGB
rayColor world background ray =
  case hit ray 0 1000 world of
    Just Hit {..} ->
      let R3 u v w = hitNormal
       in R3 (u + 1) (v + 1) (w + 1) `ctimes` 0.5
    Nothing -> background ray

render :: Hittable a => ImageConfig -> ViewportConfig -> RenderConfig -> a -> IO PPM
render ImageConfig {..} vp@ViewportConfig {..} RenderConfig {..} world =
  let cmax = 255
      rows = forM (reverse [1 .. imHeight]) $ \r -> do
        forM [1 .. imWidth] $ \c ->
          let color = do
                dx <- stdUniform
                dy <- stdUniform
                let u = (fromIntegral c + dx) / fromIntegral (imWidth - 1)
                    v = (fromIntegral r + dy) / fromIntegral (imHeight - 1)
                pure . rayColor world renderBackground $
                  Ray
                    origin
                    ( lowerLeftCorner vp
                        `plus` (horizontal `ctimes` u)
                        `plus` (vertical `ctimes` v)
                        `minus` origin
                    )
              colors = replicateM numAntialiasingSamples color
              colorSamples = evalState (sampleRVar colors) $ mkStdGen renderSeed
           in pure . rgbInt cmax . vmean $ colorSamples
   in PPM imWidth imHeight cmax <$> rows
