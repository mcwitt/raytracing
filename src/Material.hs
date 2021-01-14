module Material
  ( Material (scatter),
    Scattered (Scattered, attenuation, scattered),
    lambertian,
  )
where

import Control.Monad.Loops (iterateUntil)
import Data.RVar (RVar)
import Data.Random (uniform)
import Ray (Ray (Ray))
import Vec (R3 (..), nearZero, norm2, plus, unit)

data Scattered = Scattered
  { attenuation :: Double,
    scattered :: Ray
  }

newtype Material = Material
  { scatter :: Ray -> R3 Double -> R3 Double -> RVar Scattered
  }

uniformInUnitBall :: RVar (R3 Double)
uniformInUnitBall =
  let r = uniform (-1) 1
      u = R3 <$> r <*> r <*> r
   in iterateUntil ((< 1) . norm2) u

uniformOnUnitSphere :: RVar (R3 Double)
uniformOnUnitSphere = unit <$> uniformInUnitBall

lambertian :: Double -> Material
lambertian albedo = Material $ \_ point normal -> do
  r <- uniformOnUnitSphere
  let scatterDir = normal `plus` r
      scatterDirFixed = if not $ nearZero 1e-8 scatterDir then scatterDir else normal
      scatteredRay = Ray point scatterDirFixed
  pure $ Scattered albedo scatteredRay
