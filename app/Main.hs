module Main (main) where

import safe Hittable (Sphere (..))
import safe Lib
  ( defaultImageConfig,
    defaultViewportConfig,
    render,
  )
import safe PPM (encodeP3)
import safe Vec (R3 (..))

main :: IO ()
main = do
  let world =
        [ Sphere {spCenter = R3 0 0 (-1), spRadius = 0.5},
          Sphere {spCenter = R3 0 (-100.5) (-1), spRadius = 100}
        ]
  image <- render defaultImageConfig (defaultViewportConfig defaultImageConfig) world
  putTextLn $ encodeP3 image
