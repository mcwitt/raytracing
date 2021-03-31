module Color (RGB, RGBInt, rgbInt, uniformRGB) where

import Data.RVar (RVar)
import Data.Random (stdUniform)
import Vec (R3 (R3))

type RGB = R3 Double

type RGBInt = R3 Int

rgbInt :: Int -> RGB -> RGBInt
rgbInt cmax (R3 r g b) = R3 (f r) (f g) (f b)
  where
    f c = floor (c * fromIntegral cmax)

uniformRGB :: RVar RGB
uniformRGB = R3 <$> stdUniform <*> stdUniform <*> stdUniform
