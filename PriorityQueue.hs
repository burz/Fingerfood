module Fingerfood.PriorityQueue
( PriorityQueue
, priorityQueue
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

data Elem a p = Elem{ getElem :: a, getPrio :: p }

newtype PriorityQueue a p = PriorityQueue (FingerTree (Prio p) (Elem a p))

instance (Ord p) => Measured (Elem a p) (Prio p) where
    norm x = Prio $ getPrio x

priorityQueue :: (Ord p) => [(a, p)] -> PriorityQueue a p
priorityQueue xs = PriorityQueue $ toTree (map elem xs)
    where elem (x, p) = Elem x p

extractMax :: (Ord p) => PriorityQueue a p -> (Maybe a, PriorityQueue a p)
extractMax (PriorityQueue Empty) = (Nothing, PriorityQueue Empty)
extractMax (PriorityQueue q)     = (Just x, PriorityQueue (l >< r))
    where Split l (Elem x _) r   = splitTree (norm q <=) mempty q

