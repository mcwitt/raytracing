{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module Camera (CameraConfig (..), camera, defaultCameraConfig, getRay) where

import Data.Ratio ((%))
import Ray (Ray (Ray))
import Vec

newtype Degrees = Degrees {unDegrees :: Double} deriving newtype (Eq, Show, Num)

newtype Radians = Radians {unRadians :: Double} deriving newtype (Eq, Show, Num)

data CameraConfig = CameraConfig
  { lookFrom :: R3 Double,
    lookAt :: R3 Double,
    up :: R3 Double,
    verticalFovDegrees :: Degrees,
    aspectRatio :: Ratio Int,
    focalLength :: Double
  }

defaultCameraConfig :: CameraConfig
defaultCameraConfig =
  CameraConfig
    { lookFrom = R3 0 0 1,
      lookAt = R3 0 0 0,
      up = R3 0 1 0,
      verticalFovDegrees = Degrees 90.0,
      aspectRatio = 3 % 2,
      focalLength = 1.0
    }

data Camera = UnsafeMkCamera
  { horizontal :: R3 Double,
    vertical :: R3 Double,
    lowerLeftCorner :: R3 Double
  }

camera :: CameraConfig -> Camera
camera CameraConfig {..} =
  let w = unit (lookFrom `minus` lookAt)
      u = unitCross (unit up) w
      v = unitCross w u
      θ = degreesToRadians verticalFovDegrees
      h = tan (unRadians θ / 2)
      viewportHeight = 2.0 * h
      viewportWidth = realToFrac aspectRatio * viewportHeight
      horizontal = viewportWidth `ctimesUnit` u
      vertical = viewportHeight `ctimesUnit` v
      lowerLeftCorner = lookFrom `minus` (horizontal `divc` 2) `minus` (vertical `divc` 2) `minus` unUnit w
   in UnsafeMkCamera
        { horizontal = horizontal,
          vertical = vertical,
          lowerLeftCorner = lowerLeftCorner
        }

degreesToRadians :: Degrees -> Radians
degreesToRadians (Degrees deg) = Radians (deg * pi / 180)

getRay :: CameraConfig -> Camera -> Double -> Double -> Ray
getRay cc c s t =
  Ray
    (lookFrom cc)
    ( lowerLeftCorner c
        `plus` (s `ctimes` horizontal c)
        `plus` (t `ctimes` vertical c)
        `minus` lookFrom cc
    )
