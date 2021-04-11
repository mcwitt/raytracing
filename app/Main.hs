{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import Camera
  ( CameraConfig (aperture, focusDist, lookAt, lookFrom, up),
    defaultCameraConfig,
  )
import Color (uniformRGB)
import Control.Monad (forM)
import Data.Maybe (catMaybes)
import Data.RVar (RVar, sampleRVar)
import Data.Random (stdUniform, uniform)
import Hittable (Sphere (Sphere))
import Lib
  ( ImageConfig (..),
    RenderConfig (..),
    defaultImageConfig,
    defaultRenderConfig,
    render,
  )
import Material (dielectric, lambertian, metal)
import Options.Generic
  ( Generic,
    ParseRecord (..),
    getRecord,
    lispCaseModifiers,
    parseRecordWithModifiers,
    type (<!>) (unDefValue),
  )
import PPM (p3Lines)
import Streaming.Prelude as S (writeFile)
import Vec (R3 (..), minus, norm, times)

data Arguments = Arguments
  { imageWidth :: Int <!> "600",
    imageHeight :: Int <!> "400",
    outputFile :: FilePath <!> "output.ppm"
  }
  deriving stock (Generic, Show)

instance ParseRecord Arguments where
  parseRecord = parseRecordWithModifiers lispCaseModifiers

randomScene :: RVar [Sphere]
randomScene =
  let ground = Sphere (R3 0 (-1000) 0) 1000 (lambertian (R3 0.5 0.5 0.5))
      marbles = catMaybes <$> forM [(a, b) | a <- [-11 .. 11], b <- [-11 .. 11]] randomSphere
      bigBalls =
        [ Sphere (R3 0 1 0) 1.0 $ dielectric 1.5,
          Sphere (R3 (-4) 1 0) 1.0 $ lambertian (R3 0.4 0.2 0.1),
          Sphere (R3 4 1 0) 1.0 $ metal 0.0 (R3 0.7 0.6 0.5)
        ]
   in (++) <$> ((ground :) <$> marbles) <*> pure bigBalls
  where
    randomSphere :: (Double, Double) -> RVar (Maybe Sphere)
    randomSphere (a, b) = do
      chooseMat <- stdUniform
      center <- (\ux uz -> R3 (a + 0.9 * ux) 0.2 (b + 0.9 * uz)) <$> stdUniform <*> stdUniform
      if norm (center `minus` R3 4 0.2 0) > 0.9 then Just <$> makeSphere center chooseMat else pure Nothing

    makeSphere :: R3 Double -> Double -> RVar Sphere
    makeSphere center u
      | u < 0.80 = do
        color <- uniformRGB
        pure $ Sphere center 0.2 $ lambertian $ color `times` color
      | u < 0.95 = do
        albedo <- uniformRGB
        fuzz <- uniform 0 0.5
        pure $ Sphere center 0.2 $ metal fuzz albedo
      | otherwise = pure $ Sphere center 0.2 $ dielectric 1.5

main :: IO ()
main = do
  args :: Arguments <- getRecord "raytracer"
  let imageConfig =
        defaultImageConfig
          { imWidth = unDefValue $ imageWidth args,
            imHeight = unDefValue $ imageHeight args
          }
  let cameraConfig =
        defaultCameraConfig
          { lookFrom = R3 13 2 3,
            lookAt = R3 0 0 0,
            up = R3 0 1 0,
            focusDist = 10.0,
            aperture = 0.1
          }
      renderConfig = defaultRenderConfig {renderSamples = 500}

  scene <- sampleRVar randomScene

  let renderedPixels = render imageConfig renderConfig cameraConfig scene
      outputLines = p3Lines renderedPixels
      outputPath = unDefValue $ outputFile args

  S.writeFile outputPath outputLines
