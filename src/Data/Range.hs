{-# language PackageImports #-}

module Data.Range
  ( Range
  , singleton
  , fromBounds
  , fromSize
  , begin
  , end
  , width
  , discreteSize
  , gap
  ) where

import "base" Data.Semigroup ( Semigroup, (<>) )

-- | Inclusive range, defined in terms of its bounds.
data Range a
   = Range
     { begin :: !a
     , end   :: !a
     } deriving (Eq, Ord, Show)

singleton :: a -> Range a
singleton a = Range a a

fromBounds :: (Ord a) => a -> a -> Range a
fromBounds a1 a2
    | a1 <= a2  = Range a1 a2
    | otherwise = Range a2 a1

fromSize :: (Ord a, Enum a, Num a) => a -> a -> Range a
fromSize bound size = fromBounds bound (bound + pred size)

-- | Creates new ranges from the lowest and highest bounds of two other ranges.
instance (Ord a) => Semigroup (Range a) where
    x <> y = Range
             { begin = min (begin x) (begin y)
             , end   = max (end   x) (end   y)
             }

width :: (Num a) => Range a -> a
width (Range b e) = e - b

discreteSize :: (Enum a, Num a) => Range a -> a
discreteSize = succ . width

gap :: (Ord a, Num a) => Range a -> Range a -> a
gap x y
    | yEnd < xBegin = xBegin - yEnd - 1
    | xEnd < yBegin = yBegin - xEnd - 1
    | otherwise = 0
  where
    xBegin = begin x
    yBegin = begin y

    xEnd = end x
    yEnd = end y
