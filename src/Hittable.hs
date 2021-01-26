{-# LANGUAGE RecordWildCards #-}

module Hittable
  ( Hit (..),
    Hittable,
    Sphere (..),
    hit,
  )
where

import Material (Material, Side (..))
import Ray (Ray (Ray), at)
import Safe (minimumByMay)
import Vec (R3, dot, minus, neg, norm2, unit)

data Hit = Hit
  { hitPoint :: R3 Double,
    hitNormal :: R3 Double,
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
          t <- if d >= 0 then Just ((- b - sqrt d) / a) else Nothing
          guard (tmin <= t && t <= tmax)
          let point = ray `at` t
              outwardNormal = unit (point `minus` spCenter)
              side = if dir `dot` outwardNormal < 0 then Front else Back
          pure $
            Hit
              { hitPoint = point,
                hitNormal = case side of
                  Front -> outwardNormal
                  Back -> neg outwardNormal,
                hitSide = side,
                hitAt = t,
                hitMaterial = spMaterial
              }

instance Hittable a => Hittable [a] where
  hit ray tmin tmax xs =
    let hits = mapMaybe (hit ray tmin tmax) xs
        closestHit = minimumByMay (comparing hitAt) hits
     in closestHit
