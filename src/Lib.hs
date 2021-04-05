{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib
  ( ImageConfig (..),
    RenderConfig (..),
    render,
    defaultImageConfig,
    defaultRenderConfig,
  )
where

import Camera (CameraConfig, camera, getRay)
import Color (RGB, rgbInt)
import Data.RVar (RVar, sampleRVar)
import Data.Random (stdUniform)
import Hittable (Hit (..), Hittable, hit)
import Material (Material (scatter), Scattered (Scattered))
import PPM (PPM (..))
import Ray (Ray (Ray, rayDir))
import System.IO (hPutStr)
import Text.Printf (printf)
import Vec
  ( R3 (..),
    Unit (unUnit),
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

defaultImageConfig :: ImageConfig
defaultImageConfig =
  ImageConfig
    { imWidth = 600,
      imHeight = 400
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
        let R3 _ y _ = unUnit $ unit dir
            s = 0.5 * (y + 1.0)
         in (R3 1 1 1 `timesc` (1.0 - s)) `plus` (R3 0.5 0.7 1.0 `timesc` s),
      renderSeed = 137,
      renderMaxChildRays = 50
    }

rayColor :: Hittable a => a -> (Ray -> RGB) -> Int -> Ray -> RVar RGB
rayColor world background = go
  where
    go 0 _ = pure $ R3 0 0 0
    go n ray = case hit ray eps infinity world of
      Just Hit {..} -> do
        Scattered attenuation scatteredRay <-
          scatter hitMaterial (rayDir ray) hitPoint hitNormal hitSide
        color <- go (n - 1) scatteredRay
        pure $ color `times` attenuation
      _ -> pure $ background ray
    eps = 1e-9
    infinity = 1e9

render :: Hittable a => ImageConfig -> RenderConfig -> CameraConfig -> a -> IO PPM
render ImageConfig {..} RenderConfig {..} cameraConfig world =
  let cmax = 255
      c = camera cameraConfig
      rows = forM (reverse [1 .. imHeight]) $ \y -> do
        forM [1 .. imWidth] $ \x -> do
          hPutStr stderr $ printf "\rProgress: %d/%d" (imHeight - y + 1) imHeight
          let color = do
                dx <- stdUniform
                dy <- stdUniform
                let u = (fromIntegral x + dx) / fromIntegral (imWidth - 1)
                    v = (fromIntegral y + dy) / fromIntegral (imHeight - 1)
                ray <- getRay cameraConfig c u v
                rayColor world renderBackground renderMaxChildRays ray
              colors = replicateM renderSamples color
          rgbInt cmax . vmap sqrt . vmean <$> sampleRVar colors
   in PPM imWidth imHeight cmax <$> rows
