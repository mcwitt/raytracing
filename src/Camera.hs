{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module Camera (Camera (Camera), defaultCamera, getRay) where

import Data.Ratio ((%))
import Ray (Ray (Ray))
import Vec (R3 (..), divc, minus, plus, timesc)

newtype Degrees = Degrees {unDegrees :: Double} deriving newtype (Eq, Show, Num)

newtype Radians = Radians {unRadians :: Double} deriving newtype (Eq, Show, Num)

data Camera = Camera
  { verticalFovDegrees :: Degrees,
    aspectRatio :: Ratio Int,
    origin :: R3 Double,
    focalLength :: Double
  }

defaultCamera :: Camera
defaultCamera =
  Camera
    { verticalFovDegrees = Degrees 90.0,
      aspectRatio = 3 % 2,
      origin = R3 0.0 0.0 0.0,
      focalLength = 1.0
    }

degreesToRadians :: Degrees -> Radians
degreesToRadians (Degrees deg) = Radians (deg * pi / 180)

viewportHeight :: Camera -> Double
viewportHeight camera =
  let θ = degreesToRadians (verticalFovDegrees camera)
      h = tan (unRadians θ / 2)
   in 2.0 * h

viewportWidth :: Camera -> Double
viewportWidth c = realToFrac (aspectRatio c) * viewportHeight c

horizontal :: Camera -> R3 Double
horizontal camera = R3 (viewportWidth camera) 0 0

vertical :: Camera -> R3 Double
vertical camera = R3 0 (viewportHeight camera) 0

lowerLeftCorner :: Camera -> R3 Double
lowerLeftCorner c =
  origin c `minus` (horizontal c `divc` 2) `minus` (vertical c `divc` 2) `minus` R3 0 0 (focalLength c)

getRay :: Camera -> Double -> Double -> Ray
getRay c u v =
  Ray
    (origin c)
    ( lowerLeftCorner c
        `plus` (horizontal c `timesc` u)
        `plus` (vertical c `timesc` v)
        `minus` origin c
    )
