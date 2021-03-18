{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module Camera (Camera (..), defaultCamera, getRay, viewport) where

import Data.Ratio ((%))
import Ray (Ray (Ray))
import Vec

newtype Degrees = Degrees {unDegrees :: Double} deriving newtype (Eq, Show, Num)

newtype Radians = Radians {unRadians :: Double} deriving newtype (Eq, Show, Num)

data Camera = Camera
  { lookFrom :: R3 Double,
    lookAt :: R3 Double,
    up :: R3 Double,
    verticalFovDegrees :: Degrees,
    aspectRatio :: Ratio Int,
    focalLength :: Double
  }

defaultCamera :: Camera
defaultCamera =
  Camera
    { lookFrom = R3 0 0 1,
      lookAt = R3 0 0 0,
      up = R3 0 1 0,
      verticalFovDegrees = Degrees 90.0,
      aspectRatio = 3 % 2,
      focalLength = 1.0
    }

data Viewport = UnsafeMkViewport
  { horizontal :: R3 Double,
    vertical :: R3 Double,
    lowerLeftCorner :: R3 Double
  }

viewport :: Camera -> Viewport
viewport c@Camera {..} =
  let w = unit (lookFrom `minus` lookAt)
      u = unitCross (unit up) w
      v = unitCross w u
      horizontal = viewportWidth c `ctimesUnit` u
      vertical = viewportHeight c `ctimesUnit` v
      lowerLeftCorner = lookFrom `minus` (horizontal `divc` 2) `minus` (vertical `divc` 2) `minus` unUnit w
   in UnsafeMkViewport
        { horizontal = horizontal,
          vertical = vertical,
          lowerLeftCorner = lowerLeftCorner
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

getRay :: Camera -> Viewport -> Double -> Double -> Ray
getRay c vp s t =
  Ray
    (lookFrom c)
    ( lowerLeftCorner vp
        `plus` (s `ctimes` horizontal vp)
        `plus` (t `ctimes` vertical vp)
        `minus` lookFrom c
    )
