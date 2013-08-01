module IntervalSet
(
) where

import Data.Monoid

import FingerTree

instance (Monoid a, Monoid b) => Monoid (a, b) where
    mempty = (mempty, mempty)
    (a, b) `mappend` (a', b') = (a `mappend` a', b `mappend` b')

data Interval a = Interval{ low :: a, high :: a }

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

newtype IntervalSet a = IntervalSet (FingerTree (Key (Num a), Prio (Num a)) Interval a)

instance Measured Interval (Key (Num a), Prio (Num a)) where
    norm i = (Key $ low i, Prio $ high i)

atLeast :: (Ord a) => a -> (Key a, Prio a) -> Bool
atLeast k (_, n) = Prio k <= n

greater :: (Ord a) => a -> (Key a, Prio a) -> Bool
greater k (n, _) = n > Key k



