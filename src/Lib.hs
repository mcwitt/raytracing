{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib (render, defaultImageConfig, defaultViewportConfig, defaultRenderConfig) where

import Color (RGB, rgbInt)
import Data.RVar (RVar, sampleRVar)
import Data.Random (stdUniform)
import Data.Ratio ((%))
import Hittable (Hit (..), Hittable, hit)
import Material (Material (..), Scattered (..))
import PPM (PPM (..))
import Ray (Ray (Ray, rayDir))
import Vec
  ( R3 (..),
    divc,
    minus,
    plus,
    times,
    timesc,
    unit,
    vmap,
    vmean,
  )

data ImageConfig = ImageConfig
  { imWidth :: Int,
    imHeight :: Int
  }

aspectRatio :: ImageConfig -> Ratio Int
aspectRatio (ImageConfig w h) = w % h

defaultImageConfig :: ImageConfig
defaultImageConfig = ImageConfig {imWidth = 400, imHeight = 225}

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
  { renderSamples :: Int,
    renderBackground :: Ray -> RGB,
    renderSeed :: Int,
    renderMaxChildRays :: Int
  }

defaultRenderConfig :: RenderConfig
defaultRenderConfig =
  RenderConfig
    { renderSamples = 30,
      renderBackground = \(Ray _ dir) ->
        let R3 _ y _ = unit dir
            s = 0.5 * (y + 1.0)
         in (R3 1 1 1 `timesc` (1.0 - s)) `plus` (R3 0.5 0.7 1.0 `timesc` s),
      renderSeed = 137,
      renderMaxChildRays = 50
    }

viewportWidth :: ImageConfig -> ViewportConfig -> Double
viewportWidth ic ViewportConfig {..} = realToFrac (aspectRatio ic) * viewportHeight

lowerLeftCorner :: ViewportConfig -> R3 Double
lowerLeftCorner ViewportConfig {..} =
  origin `minus` (horizontal `divc` 2) `minus` (vertical `divc` 2) `minus` R3 0 0 focalLength

rayColor :: Hittable a => a -> (Ray -> RGB) -> Int -> Ray -> RVar RGB
rayColor world background = go
  where
    go 0 _ = pure $ R3 0 0 0
    go n ray = case hit ray eps infinity world of
      Just Hit {..} -> do
        Scattered attenuation scatteredRay <- scatter hitMaterial (rayDir ray) hitPoint hitNormal
        color <- go (n - 1) scatteredRay
        pure $ color `times` attenuation
      _ -> pure $ background ray
    eps = 1e-9
    infinity = 1e9

render :: Hittable a => ImageConfig -> ViewportConfig -> RenderConfig -> a -> IO PPM
render ImageConfig {..} vp@ViewportConfig {..} RenderConfig {..} world =
  let cmax = 255
      rows = forM (reverse [1 .. imHeight]) $ \r -> do
        forM [1 .. imWidth] $ \c -> do
          let color = do
                dx <- stdUniform
                dy <- stdUniform
                let u = (fromIntegral c + dx) / fromIntegral (imWidth - 1)
                    v = (fromIntegral r + dy) / fromIntegral (imHeight - 1)
                rayColor world renderBackground renderMaxChildRays $
                  Ray
                    origin
                    ( lowerLeftCorner vp
                        `plus` (horizontal `timesc` u)
                        `plus` (vertical `timesc` v)
                        `minus` origin
                    )
              colors = replicateM renderSamples color
          rgbInt cmax . vmap sqrt . vmean <$> sampleRVar colors
   in PPM imWidth imHeight cmax <$> rows
