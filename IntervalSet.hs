module IntervalSet
( Interval(..)
, IntervalSet
, intervalSet
, intervalSearch
, intervalMatch
) where

import Data.Monoid

import FingerTree

data Interval a = Interval{ low :: a, high :: a } deriving Show

data Key a = NoKey | Key a deriving (Eq, Ord)

instance Monoid (Key a) where
    mempty            = NoKey
    k `mappend` NoKey = k
    _ `mappend` k     = k

data Prio a = MInfty | Prio a deriving (Eq, Ord)

instance (Ord a) => Monoid (Prio a) where
    mempty                  = MInfty
    MInfty `mappend` p      = p
    p `mappend` MInfty      = p
    Prio m `mappend` Prio n = Prio $ m `max` n

newtype IntervalSet a = IntervalSet (FingerTree (Key a, Prio a) (Interval a))

instance (Ord a) => Measured (Interval a) (Key a, Prio a) where
    norm i = (Key $ low i, Prio $ high i)

intervalSet :: (Ord a) => [(a, a)] -> IntervalSet a
intervalSet xs = IntervalSet $ toTree (map interval xs)
    where interval (l, h) = Interval l h

atLeast :: (Ord a) => a -> (Key a, Prio a) -> Bool
atLeast k (_, n) = Prio k <= n

greater :: (Ord a) => a -> (Key a, Prio a) -> Bool
greater k (n, _) = n > Key k

intervalSearch :: (Ord a) => IntervalSet a -> Interval a -> Maybe (Interval a)
intervalSearch (IntervalSet t) i
    | atLeast (low i) (norm t) && low x <= high i = Just x
    | otherwise                                      = Nothing
    where Split _ x _ = splitTree (atLeast (low i)) mempty t

intervalMatch :: (Ord a) => IntervalSet a -> Interval a -> [Interval a]
intervalMatch (IntervalSet t) i = matches $ takeUntil (greater $ high i) t
    where matches xs = case viewL $ dropUntil (atLeast (low i)) xs of
                          NilL -> []
                          ConsL x xs' -> x : matches xs'

