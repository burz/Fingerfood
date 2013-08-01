module PriorityQueue
( PriorityQueue
, toPriorityQueue
, extractMax
) where

import Data.Monoid

import FingerTree

data Prio a = MInfty | Prio a deriving (Eq, Ord)

instance (Ord a) => Monoid (Prio a) where
    mempty                  = MInfty
    MInfty `mappend` p      = p
    p `mappend` MInfty      = p
    Prio m `mappend` Prio n = Prio $ m `max` n

newtype Elem a = Elem{ getElem :: a }

newtype PriorityQueue a = PriorityQueue (FingerTree (Prio a) (Elem a))

instance (Ord a) => Measured (Elem a) (Prio a) where
    norm (Elem x) = Prio x

toPriorityQueue :: (Ord a) => [a] -> PriorityQueue a
toPriorityQueue xs = PriorityQueue $ toTree (map Elem xs)

extractMax :: (Ord a) => PriorityQueue a -> (a, PriorityQueue a)
extractMax (PriorityQueue q) = (x, PriorityQueue (l >< r))
    where Split l (Elem x) r = splitTree (norm q <=) mempty q

