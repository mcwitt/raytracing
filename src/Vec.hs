{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module Vec
  ( R3 (R3),
    cross,
    cdiv,
    ctimes,
    dot,
    vmean,
    minus,
    nearZero,
    neg,
    norm2,
    plus,
    timesc,
    unit,
    vmap,
  )
where

import Data.List (foldl1')

data R3 a where
  R3 :: Real a => a -> a -> a -> R3 a

deriving stock instance Show a => Show (R3 a)

neg :: R3 a -> R3 a
neg (R3 x y z) = R3 (- x) (- y) (- z)

plus, minus :: R3 a -> R3 a -> R3 a
plus (R3 x1 y1 z1) (R3 x2 y2 z2) = R3 (x1 + x2) (y1 + y2) (z1 + z2)
minus u v = u `plus` neg v

vmap :: (a -> a) -> R3 a -> R3 a
vmap f (R3 x y z) = R3 (f x) (f y) (f z)

vfoldMap :: Monoid m => (a -> m) -> R3 a -> m
vfoldMap f (R3 x y z) = f x <> f y <> f z

timesc :: Num a => a -> R3 a -> R3 a
timesc c = vmap (c *)

ctimes :: Num a => R3 a -> a -> R3 a
ctimes = flip timesc

divc :: Fractional a => a -> R3 a -> R3 a
divc c = vmap (/ c)

cdiv :: Fractional a => R3 a -> a -> R3 a
cdiv = flip divc

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

vsum :: [R3 a] -> R3 a
vsum = foldl1' plus

vmean :: Fractional a => [R3 a] -> R3 a
vmean xs = vsum xs `cdiv` fromIntegral (length xs)

nearZero :: Double -> R3 Double -> Bool
nearZero eps = getAll . vfoldMap (All . (< eps) . abs)
