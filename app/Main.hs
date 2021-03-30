{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import Camera
import Data.Text.IO as TIO (writeFile)
import Hittable (Sphere (..))
import Lib
  ( ImageConfig (..),
    RenderConfig (..),
    defaultRenderConfig,
    render,
  )
import Material (dielectric, lambertian, metal)
import Options.Generic
  ( ParseRecord (..),
    getRecord,
    lispCaseModifiers,
    parseRecordWithModifiers,
    type (<!>) (unDefValue),
  )
import PPM (encodeP3)
import Vec (R3 (..), minus, norm)

data Arguments = Arguments
  { imageWidth :: Int <!> "450",
    imageHeight :: Int <!> "225",
    outputFile :: FilePath <!> "output.ppm"
  }
  deriving stock (Generic, Show)

instance ParseRecord Arguments where
  parseRecord = parseRecordWithModifiers lispCaseModifiers

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
              spMaterial = lambertian $ R3 0.1 0.2 0.5
            },
          Sphere
            { spCenter = R3 (-1) 0 (-1),
              spRadius = 0.5,
              spMaterial = dielectric 1.5
            },
          Sphere
            { spCenter = R3 1 0 (-1),
              spRadius = 0.5,
              spMaterial = metal 0 $ R3 0.8 0.6 0.2
            }
        ]
  args :: Arguments <- getRecord "raytracer"
  let imageConfig =
        ImageConfig
          { imWidth = unDefValue $ imageWidth args,
            imHeight = unDefValue $ imageHeight args
          }
  let cameraConfig =
        defaultCameraConfig
          { lookFrom = R3 3 3 2,
            lookAt = R3 0 0 (-1),
            focusDist = norm (lookFrom cameraConfig `minus` lookAt cameraConfig),
            verticalFovDegrees = 20,
            aperture = 2.0
          }
      renderConfig = defaultRenderConfig {renderSamples = 10}
  image <-
    render
      imageConfig
      renderConfig
      cameraConfig
      world
  TIO.writeFile (unDefValue $ outputFile args) $ encodeP3 image
