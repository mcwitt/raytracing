{-# LANGUAGE RecordWildCards #-}

module Hittable
  ( Hit (..),
    Hittable,
    Sphere (..),
    hit,
  )
where

import Control.Monad (guard)
import Data.Maybe (mapMaybe)
import Data.Ord (comparing)
import Material (Material, Side (..))
import Ray (Ray (Ray), at)
import Safe (minimumByMay)
import Vec (R3, Unit (..), dot, minus, norm2, unit, unitNeg)

data Hit = Hit
  { hitPoint :: R3 Double,
    hitNormal :: Unit Double,
    hitSide :: Side,
    hitAt :: Double,
    hitMaterial :: Material
  }

class Hittable a where
  hit :: Ray -> Double -> Double -> a -> Maybe Hit

data Sphere = Sphere
  { spCenter :: R3 Double,
    spRadius :: Double,
    spMaterial :: Material
  }

instance Hittable Sphere where
  hit ray@(Ray orig dir) tmin tmax Sphere {..} =
    let oc = orig `minus` spCenter
        a = norm2 dir
        b = oc `dot` dir
        c = norm2 oc - spRadius ** 2
        d = b ** 2 - a * c
     in do
          guard (d >= 0)
          let t = (- b - sqrt d) / a
          guard (tmin <= t && t <= tmax)
          let point = ray `at` t
              outwardNormal = point `minus` spCenter
              side = if dir `dot` outwardNormal < 0 then Front else Back
              unitOutwardNormal = unit outwardNormal
          pure $
            Hit
              { hitPoint = point,
                hitNormal = case side of
                  Front -> unitOutwardNormal
                  Back -> unitNeg unitOutwardNormal,
                hitSide = side,
                hitAt = t,
                hitMaterial = spMaterial
              }

instance Hittable a => Hittable [a] where
  hit ray tmin tmax xs =
    let hits = mapMaybe (hit ray tmin tmax) xs
        closestHit = minimumByMay (comparing hitAt) hits
     in closestHit
