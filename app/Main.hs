module Main (main) where

import Hittable (Sphere (..))
import Lib
  ( defaultImageConfig,
    defaultRenderConfig,
    defaultViewportConfig,
    render,
  )
import Material (lambertian, metal)
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
              spMaterial = lambertian $ R3 0.7 0.3 0.3
            },
          Sphere
            { spCenter = R3 (-1) 0 (-1),
              spRadius = 0.5,
              spMaterial = metal $ R3 0.8 0.8 0.8
            },
          Sphere
            { spCenter = R3 1 0 (-1),
              spRadius = 0.5,
              spMaterial = metal $ R3 0.8 0.6 0.2
            }
        ]
  image <-
    render
      defaultImageConfig
      (defaultViewportConfig defaultImageConfig)
      defaultRenderConfig
      world
  putTextLn $ encodeP3 image
