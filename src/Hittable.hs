module Hittable (Hit (..), Sphere (..), hit) where

import Ray (Ray (Ray), at)
import Vec (R3, dot, minus, neg, norm2, unit)

data Hit = Hit
  { hitPoint :: R3 Double,
    hitNormal :: R3 Double,
    hitFront :: Bool,
    hitAt :: Double
  }

class Hittable a where
  hit :: a -> Ray -> Double -> Double -> Maybe Hit

data Sphere = Sphere
  { spCenter :: R3 Double,
    spRadius :: Double
  }

instance Hittable Sphere where
  hit (Sphere center radius) ray@(Ray orig dir) tmin tmax =
    let oc = orig `minus` center
        a = norm2 dir
        b = oc `dot` dir
        c = norm2 oc - radius ** 2
        d = b ** 2 - a * c
     in do
          t <- if d >= 0 then Just ((- b - sqrt d) / a) else Nothing
          guard (tmin <= t && t <= tmax)
          let point = ray `at` t
              outwardNormal = unit (point `minus` center)
              isFront = dir `dot` outwardNormal < 0
          pure $
            Hit
              { hitPoint = point,
                hitNormal = if isFront then outwardNormal else neg outwardNormal,
                hitFront = isFront,
                hitAt = t
              }
