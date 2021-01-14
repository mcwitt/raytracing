module Ray
  ( Ray (..),
    at,
  )
where

import Vec (R3, plus, timesc)

data Ray = Ray {rayOrig :: R3 Double, rayDir :: R3 Double}

at :: Ray -> Double -> R3 Double
at (Ray orig dir) t = orig `plus` (dir `timesc` t)
