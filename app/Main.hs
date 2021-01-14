module Main (main) where

import Hittable (Sphere (..))
import Lib
  ( defaultImageConfig,
    defaultRenderConfig,
    defaultViewportConfig,
    render,
  )
import Material (lambertian)
import PPM (encodeP3)
import Vec (R3 (..))

main :: IO ()
main = do
  let world =
        [ Sphere {spCenter = R3 0 0 (-1), spRadius = 0.5, spMaterial = lambertian 0.5},
          Sphere {spCenter = R3 0 (-100.5) (-1), spRadius = 100, spMaterial = lambertian 0.5}
        ]
  image <-
    render
      defaultImageConfig
      (defaultViewportConfig defaultImageConfig)
      defaultRenderConfig
      world
  putTextLn $ encodeP3 image
