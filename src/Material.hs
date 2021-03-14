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
    Unit (unUnit),
    ctimes,
    dot,
    minus,
    nearZero,
    norm2,
    plus,
    unit,
    unitCos,
  )

data Scattered = Scattered
  { attenuation :: RGB,
    scattered :: Ray
  }

data Side = Front | Back

newtype Material = Material
  { scatter :: RGB -> R3 Double -> Unit Double -> Side -> RVar Scattered
  }

uniformInUnitBall :: RVar (R3 Double)
uniformInUnitBall =
  let r = uniform (-1) 1
      u = R3 <$> r <*> r <*> r
   in iterateUntil ((< 1) . norm2) u

uniformOnUnitSphere :: RVar (Unit Double)
uniformOnUnitSphere = unit <$> uniformInUnitBall

lambertian :: RGB -> Material
lambertian albedo = Material $ \_ point unitNormal _ -> do
  r <- uniformOnUnitSphere
  let normal = unUnit unitNormal
      scatterDir = normal `plus` unUnit r
      scatterDirFixed = if not $ nearZero 1e-8 scatterDir then scatterDir else normal
      scatteredRay = Ray point scatterDirFixed
  pure $ Scattered albedo scatteredRay

metal :: Double -> RGB -> Material
metal fuzz albedo = Material $ \rayDir point normal _ -> do
  let reflected = reflect (unit rayDir) normal
  r <- uniformInUnitBall
  let scatteredRay = Ray point (reflected `plus` (fuzz `ctimes` r))
  pure $ Scattered albedo scatteredRay

dielectric :: Double -> Material
dielectric ir = Material $ \rayDir point unitNormal side -> do
  let ratio = case side of
        Front -> 1.0 / ir
        Back -> ir
      unitDir = unit rayDir
      cosθ = - unitCos unitDir unitNormal
      sinθ = sqrt (1.0 - cosθ ** 2)
      cannotRefract = ratio * sinθ > 1.0
      reflectance =
        let r0 = ((1 - ratio) / (1 + ratio)) ** 2
         in r0 + (1 - r0) * ((1 - cosθ) ** 5)
  r <- uniform 0 1
  let newDir =
        if cannotRefract || r < reflectance
          then reflect unitDir unitNormal
          else refract unitDir unitNormal ratio
      scatteredRay = Ray point newDir
  pure $ Scattered (R3 1 1 1) scatteredRay

reflect :: Num a => Unit a -> Unit a -> R3 a
reflect uv un =
  let v = unUnit uv
      n = unUnit un
   in v `minus` ((2 * v `dot` n) `ctimes` n)

refract :: Floating a => Unit a -> Unit a -> a -> R3 a
refract uv un ratio =
  let v = unUnit uv
      n = unUnit un
      cosθ = - v `dot` n
      rperp = ratio `ctimes` (v `plus` (cosθ `ctimes` n))
      rpar = (- sqrt (abs (1 - norm2 rperp))) `ctimes` n
   in rperp `plus` rpar
