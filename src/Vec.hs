{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module Vec where

data R3 a where
  R3 :: Num a => a -> a -> a -> R3 a

deriving instance Show a => Show (R3 a)

neg :: Floating a => R3 a -> R3 a
neg (R3 x y z) = R3 (- x) (- y) (- z)

plus :: Floating a => R3 a -> R3 a -> R3 a
plus (R3 x1 y1 z1) (R3 x2 y2 z2) = R3 (x1 + x2) (y1 + y2) (z1 + z2)

ctimes, cdiv :: Floating a => R3 a -> a -> R3 a
ctimes (R3 x y z) c = R3 (c * x) (c * y) (c * z)
cdiv u c = ctimes u (1 / c)

dot, dot2 :: Floating a => R3 a -> R3 a -> a
dot2 (R3 x1 y1 z1) (R3 x2 y2 z2) = x1 * x2 + y1 * y2 + z1 * z2
dot u = sqrt . dot2 u

norm2 :: Floating a => R3 a -> a
norm2 u = dot2 u u

norm :: Floating a => R3 a -> a
norm u = dot u u

cross :: Floating a => R3 a -> R3 a -> R3 a
cross (R3 x1 y1 z1) (R3 x2 y2 z2) =
  R3 (y1 * (z2 - x2)) (z1 * (x2 - y2)) (x1 * (y2 - z2))
