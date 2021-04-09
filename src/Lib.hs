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
import Color (RGB)
import Data.RVar (RVar, sampleRVar)
import Data.Random (stdUniform)
import Hittable (Hit (..), Hittable, hit)
import Material (Material (scatter), Scattered (Scattered))
import PPM (StreamingPPM (..))
import Pipes (Producer, each, for, yield)
import Ray (Ray (Ray, rayDir))
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

renderedPixels :: Hittable a => ImageConfig -> RenderConfig -> CameraConfig -> a -> Producer RGB IO ()
renderedPixels ImageConfig {..} RenderConfig {..} cameraConfig scene =
  let c = camera cameraConfig
      pixels = do y <- reverse [1 .. imHeight]; x <- [1 .. imWidth]; pure (x, y)
      pixelValues = for (each pixels) $ \(x, y) -> do
        lift $ putStr $ printf "\rProgress: %d/%d" (imHeight - y + 1) imHeight
        let color :: RVar RGB = do
              dx <- stdUniform
              dy <- stdUniform
              let u = (fromIntegral x + dx) / fromIntegral (imWidth - 1)
                  v = (fromIntegral y + dy) / fromIntegral (imHeight - 1)
              ray <- getRay cameraConfig c u v
              rayColor scene renderBackground renderMaxChildRays ray
            colors = replicateM renderSamples color
        rawSample <- lift $ sampleRVar colors
        let gammaCorrected = vmap sqrt $ vmean rawSample
        yield gammaCorrected
   in pixelValues

render :: Hittable a => ImageConfig -> RenderConfig -> CameraConfig -> a -> StreamingPPM IO
render imageConfig renderConfig cameraConfig scene =
  StreamingPPM (imWidth imageConfig) (imHeight imageConfig) 255 $
    renderedPixels imageConfig renderConfig cameraConfig scene
