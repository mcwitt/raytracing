{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib (render, defaultImageConfig, defaultViewportConfig) where

import safe Color (RGB, rgbInt)
import safe Data.Ratio ((%))
import safe Hittable
  ( Hit (Hit, hitAt, hitNormal, hitPoint),
    Sphere (Sphere, spCenter, spRadius),
    hit,
  )
import safe PPM (PPM (PPM))
import safe Ray (Ray (Ray))
import safe System.IO (hPutStr)
import safe Text.Printf (printf)
import safe Vec (R3 (R3), cdiv, ctimes, minus, plus, unit)

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

viewportWidth :: ImageConfig -> ViewportConfig -> Double
viewportWidth ic ViewportConfig {..} = realToFrac (aspectRatio ic) * viewportHeight

lowerLeftCorner :: ViewportConfig -> R3 Double
lowerLeftCorner ViewportConfig {..} =
  origin `minus` (horizontal `cdiv` 2) `minus` (vertical `cdiv` 2) `minus` R3 0 0 focalLength

rayColor :: Ray -> RGB
rayColor ray@(Ray _ dir) =
  let R3 _ y _ = unit dir
      s = 0.5 * (y + 1.0)
      world =
        [ Sphere {spCenter = R3 0 0 (-1), spRadius = 0.5},
          Sphere {spCenter = R3 0 (-100.5) (-1), spRadius = 100}
        ]
   in case hit ray 0 1000 world of
        Just Hit {..} ->
          let R3 u v w = hitNormal
           in R3 (u + 1) (v + 1) (w + 1) `ctimes` 0.5
        Nothing -> (R3 1 1 1 `ctimes` (1.0 - s)) `plus` (R3 0.5 0.7 1.0 `ctimes` s)

render :: ImageConfig -> ViewportConfig -> IO PPM
render ImageConfig {..} vp@ViewportConfig {..} =
  let cmax = 255
      rows = forM (reverse [1 .. imHeight]) $ \r -> do
        hPutStr stderr $ printf "\rProgress: %d/%d" r imHeight
        forM [1 .. imWidth] $ \c ->
          let u = fromIntegral c / fromIntegral imWidth
              v = fromIntegral r / fromIntegral imHeight
           in pure . rgbInt cmax . rayColor $
                Ray
                  origin
                  ( lowerLeftCorner vp
                      `plus` (horizontal `ctimes` u)
                      `plus` (vertical `ctimes` v)
                      `minus` origin
                  )
   in PPM imWidth imHeight cmax <$> rows
