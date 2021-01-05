module Ray (Ray (Ray, rayDir, rayOrig), at) where

import Vec (R3, ctimes, plus)

data Ray = Ray {rayOrig :: R3 Double, rayDir :: R3 Double}

at :: Ray -> Double -> R3 Double
at (Ray orig dir) t = orig `plus` (dir `ctimes` t)
