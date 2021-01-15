module Material
  ( Material (..),
    Scattered (..),
    lambertian,
    metal,
  )
where

import Color (RGB)
import Control.Monad.Loops (iterateUntil)
import Data.RVar (RVar)
import Data.Random (uniform)
import Ray (Ray (Ray))
import Vec
  ( R3 (..),
    ctimes,
    dot,
    minus,
    nearZero,
    norm2,
    plus,
    unit,
  )

data Scattered = Scattered
  { attenuation :: RGB,
    scattered :: Ray
  }

newtype Material = Material
  { scatter :: RGB -> R3 Double -> R3 Double -> RVar Scattered
  }

uniformInUnitBall :: RVar (R3 Double)
uniformInUnitBall =
  let r = uniform (-1) 1
      u = R3 <$> r <*> r <*> r
   in iterateUntil ((< 1) . norm2) u

uniformOnUnitSphere :: RVar (R3 Double)
uniformOnUnitSphere = unit <$> uniformInUnitBall

lambertian :: RGB -> Material
lambertian albedo = Material $ \_ point normal -> do
  r <- uniformOnUnitSphere
  let scatterDir = normal `plus` r
      scatterDirFixed = if not $ nearZero 1e-8 scatterDir then scatterDir else normal
      scatteredRay = Ray point scatterDirFixed
  pure $ Scattered albedo scatteredRay

metal :: Double -> RGB -> Material
metal fuzz albedo = Material $ \rayDir point normal -> do
  let scatterDir = reflect (unit rayDir) normal
  r <- uniformInUnitBall
  let scatteredRay = Ray point (scatterDir `plus` (fuzz `ctimes` r))
  pure $ Scattered albedo scatteredRay
  where
    reflect v n = v `minus` ((2 * v `dot` n) `ctimes` n)
