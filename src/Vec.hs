{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}

module Vec
  ( R3 (R3),
    Unit (unUnit),
    cross,
    cdiv,
    ctimes,
    ctimesUnit,
    divc,
    dot,
    vmean,
    minus,
    nearZero,
    neg,
    norm,
    norm2,
    plus,
    times,
    timesc,
    unit,
    unitCos,
    unitCross,
    unitNeg,
    vmap,
  )
where

import Data.List (foldl1')
import Data.Monoid (All (All, getAll))

data R3 a where
  R3 :: Real a => !a -> !a -> !a -> R3 a

deriving stock instance Eq a => Eq (R3 a)

deriving stock instance Show a => Show (R3 a)

neg :: R3 a -> R3 a
neg (R3 x y z) = R3 (- x) (- y) (- z)

vzipWith :: (a -> a -> a) -> R3 a -> R3 a -> R3 a
vzipWith f (R3 x1 y1 z1) (R3 x2 y2 z2) = R3 (x1 `f` x2) (y1 `f` y2) (z1 `f` z2)

plus, minus, times :: Num a => R3 a -> R3 a -> R3 a
plus = vzipWith (+)
minus = vzipWith (-)
times = vzipWith (*)

vmap :: (a -> a) -> R3 a -> R3 a
vmap f (R3 x y z) = R3 (f x) (f y) (f z)

vfoldMap :: Monoid m => (a -> m) -> R3 a -> m
vfoldMap f (R3 x y z) = f x <> f y <> f z

ctimes :: Num a => a -> R3 a -> R3 a
ctimes c = vmap (c *)

timesc :: Num a => R3 a -> a -> R3 a
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
  R3 (y1 * z2 - z1 * y2) (z1 * x2 - x1 * z2) (x1 * y2 - y1 * x2)

vsum :: Num a => [R3 a] -> R3 a
vsum = foldl1' plus

vmean :: Fractional a => [R3 a] -> R3 a
vmean xs = vsum xs `divc` fromIntegral (length xs)

nearZero :: Double -> R3 Double -> Bool
nearZero eps = getAll . vfoldMap (All . (< eps) . abs)

newtype Unit a = UnsafeMkUnit {unUnit :: R3 a}

unit :: Floating a => R3 a -> Unit a
unit u = UnsafeMkUnit (u `divc` norm u)

unitCos :: Unit a -> Unit a -> a
unitCos u v = unUnit u `dot` unUnit v

unitCross :: Unit a -> Unit a -> Unit a
unitCross u v = UnsafeMkUnit (unUnit u `cross` unUnit v)

unitNeg :: Unit a -> Unit a
unitNeg = UnsafeMkUnit . neg . unUnit

ctimesUnit :: Num a => a -> Unit a -> R3 a
ctimesUnit c = (c `ctimes`) . unUnit
