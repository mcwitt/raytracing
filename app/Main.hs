module Main (main) where

import Hittable (Sphere (..))
import Lib
  ( ImageConfig (..),
    defaultImageConfig,
    defaultRenderConfig,
    defaultViewportConfig,
    render,
  )
import Material (dielectric, lambertian, metal)
import PPM (encodeP3)
import Vec (R3 (..))

main :: IO ()
main = do
  let world =
        [ Sphere
            { spCenter = R3 0 (-100.5) (-1),
              spRadius = 100,
              spMaterial = lambertian $ R3 0.8 0.8 0.0
            },
          Sphere
            { spCenter = R3 0 0 (-1),
              spRadius = 0.5,
              spMaterial = dielectric 1.5
            },
          Sphere
            { spCenter = R3 (-1) 0 (-1),
              spRadius = 0.5,
              spMaterial = dielectric 1.5
            },
          Sphere
            { spCenter = R3 1 0 (-1),
              spRadius = 0.5,
              spMaterial = metal 1.0 $ R3 0.8 0.6 0.2
            }
        ]
  let imageConfig = defaultImageConfig {imWidth = 800, imHeight = 450}
  image <-
    render
      imageConfig
      (defaultViewportConfig imageConfig)
      defaultRenderConfig
      world
  putTextLn $ encodeP3 image
