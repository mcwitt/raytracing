{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}

module Vec
  ( R3 (R3),
    cross,
    cdiv,
    ctimes,
    divc,
    dot,
    vmean,
    minus,
    nearZero,
    neg,
    norm2,
    plus,
    times,
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

elementwise :: (Num a => a -> a -> a) -> R3 a -> R3 a -> R3 a
elementwise f (R3 x1 y1 z1) (R3 x2 y2 z2) = R3 (x1 `f` x2) (y1 `f` y2) (z1 `f` z2)

plus, minus, times :: R3 a -> R3 a -> R3 a
plus = elementwise (+)
minus = elementwise (-)
times = elementwise (*)

vmap :: (Num a => a -> a) -> R3 a -> R3 a
vmap f (R3 x y z) = R3 (f x) (f y) (f z)

vfoldMap :: Monoid m => (a -> m) -> R3 a -> m
vfoldMap f (R3 x y z) = f x <> f y <> f z

ctimes :: a -> R3 a -> R3 a
ctimes c = vmap (c *)

timesc :: R3 a -> a -> R3 a
timesc = flip ctimes

cdiv :: Fractional a => a -> R3 a -> R3 a
cdiv c = vmap (/ c)

divc :: Fractional a => R3 a -> a -> R3 a
divc = flip cdiv

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
unit u = u `divc` norm u

vsum :: [R3 a] -> R3 a
vsum = foldl1' plus

vmean :: Fractional a => [R3 a] -> R3 a
vmean xs = vsum xs `divc` fromIntegral (length xs)

nearZero :: Double -> R3 Double -> Bool
nearZero eps = getAll . vfoldMap (All . (< eps) . abs)
