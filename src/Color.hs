module Color where

type RGB = (Double, Double, Double)

type RGBInt = (Int, Int, Int)

rgbInt :: Int -> RGB -> RGBInt
rgbInt cmax (r, g, b) = (f r, f g, f b)
  where
    f c = floor (c * fromIntegral cmax)
