module Material
  ( Material (..),
    Scattered (..),
    Side (..),
    dielectric,
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

data Side = Front | Back

newtype Material = Material
  { scatter :: RGB -> R3 Double -> R3 Double -> Side -> RVar Scattered
  }

uniformInUnitBall :: RVar (R3 Double)
uniformInUnitBall =
  let r = uniform (-1) 1
      u = R3 <$> r <*> r <*> r
   in iterateUntil ((< 1) . norm2) u

uniformOnUnitSphere :: RVar (R3 Double)
uniformOnUnitSphere = unit <$> uniformInUnitBall

lambertian :: RGB -> Material
lambertian albedo = Material $ \_ point normal _ -> do
  r <- uniformOnUnitSphere
  let scatterDir = normal `plus` r
      scatterDirFixed = if not $ nearZero 1e-8 scatterDir then scatterDir else normal
      scatteredRay = Ray point scatterDirFixed
  pure $ Scattered albedo scatteredRay

metal :: Double -> RGB -> Material
metal fuzz albedo = Material $ \rayDir point normal _ -> do
  let reflected = reflect (unit rayDir) normal
  r <- uniformInUnitBall
  let scatteredRay = Ray point (reflected `plus` (fuzz `ctimes` r))
  pure $ Scattered albedo scatteredRay
  where
    reflect u n = u `minus` ((2 * u `dot` n) `ctimes` n)

dielectric :: Double -> Material
dielectric ir = Material $ \rayDir point normal side ->
  let ratio = case side of
        Front -> 1.0 / ir
        Back -> ir
      scatteredRay = Ray point $ refract (unit rayDir) normal ratio
   in pure $ Scattered (R3 1 1 1) scatteredRay
  where
    refract u n ratio =
      let cosTheta = - u `dot` n
          rperp = ratio `ctimes` (u `plus` (cosTheta `ctimes` n))
          rpar = (- sqrt (abs (1 - norm2 rperp))) `ctimes` n
       in rperp `plus` rpar
