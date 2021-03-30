{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module Camera (CameraConfig (..), camera, defaultCameraConfig, getRay) where

import Control.Monad.Loops (iterateUntil)
import Data.RVar (RVar)
import Data.Random (uniform)
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
    aperture :: Double,
    focusDist :: Double
  }

defaultCameraConfig :: CameraConfig
defaultCameraConfig =
  CameraConfig
    { lookFrom = R3 0 0 1,
      lookAt = R3 0 0 0,
      up = R3 0 1 0,
      verticalFovDegrees = Degrees 90.0,
      aspectRatio = 3 % 2,
      aperture = 1.0,
      focusDist = 1.0
    }

data Camera = UnsafeMkCamera
  { uhat :: Unit Double,
    vhat :: Unit Double,
    what :: Unit Double,
    horizontal :: R3 Double,
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
      horizontal = (focusDist * viewportWidth) `ctimesUnit` u
      vertical = (focusDist * viewportHeight) `ctimesUnit` v
      lowerLeftCorner =
        lookFrom
          `minus` (horizontal `divc` 2)
          `minus` (vertical `divc` 2)
          `minus` (focusDist `ctimesUnit` w)
   in UnsafeMkCamera
        { uhat = u,
          vhat = v,
          what = w,
          horizontal = horizontal,
          vertical = vertical,
          lowerLeftCorner = lowerLeftCorner
        }

degreesToRadians :: Degrees -> Radians
degreesToRadians (Degrees deg) = Radians (deg * pi / 180)

getRay :: CameraConfig -> Camera -> Double -> Double -> RVar Ray
getRay cc c s t = do
  R3 x y _ <- uniformInUnitDisk
  let offset = (x `ctimesUnit` uhat c) `plus` (y `ctimesUnit` vhat c)
  pure $
    Ray
      (lookFrom cc `plus` offset)
      ( lowerLeftCorner c
          `plus` (s `ctimes` horizontal c)
          `plus` (t `ctimes` vertical c)
          `minus` lookFrom cc
          `minus` offset
      )
  where
    uniformInUnitDisk :: RVar (R3 Double)
    uniformInUnitDisk =
      let u = uniform (-1) 1
          r = R3 <$> u <*> u <*> pure 0
       in iterateUntil ((< 1) . norm2) r
