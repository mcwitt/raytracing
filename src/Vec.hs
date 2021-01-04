{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module Vec where

data R3 a where
  R3 :: Real a => a -> a -> a -> R3 a

deriving instance Show a => Show (R3 a)

neg :: R3 a -> R3 a
neg (R3 x y z) = R3 (- x) (- y) (- z)

plus, minus :: R3 a -> R3 a -> R3 a
plus (R3 x1 y1 z1) (R3 x2 y2 z2) = R3 (x1 + x2) (y1 + y2) (z1 + z2)
minus u v = u `plus` neg v

ctimes :: R3 a -> a -> R3 a
ctimes (R3 x y z) c = R3 (c * x) (c * y) (c * z)

cdiv :: Fractional a => R3 a -> a -> R3 a
cdiv u c = ctimes u (1 / c)

dot :: R3 a -> R3 a -> a
dot (R3 x1 y1 z1) (R3 x2 y2 z2) = x1 * x2 + y1 * y2 + z1 * z2

norm2 :: R3 a -> a
norm2 u = dot u u

norm :: Floating a => R3 a -> a
norm = sqrt . norm2

cross :: R3 a -> R3 a -> R3 a
cross (R3 x1 y1 z1) (R3 x2 y2 z2) =
  R3 (y1 * (z2 - x2)) (z1 * (x2 - y2)) (x1 * (y2 - z2))

unit :: Floating a => R3 a -> R3 a
unit u = u `cdiv` norm u
