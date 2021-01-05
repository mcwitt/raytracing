module Hittable (Hit (..), Sphere (..), hit) where

import Ray (Ray (Ray), at)
import Vec (R3, dot, minus, norm2, unit)

data Hit = Hit
  { hitPoint :: R3 Double,
    hitNormal :: R3 Double,
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
              normal = unit (point `minus` center)
          pure $ Hit {hitPoint = point, hitNormal = normal, hitAt = t}
