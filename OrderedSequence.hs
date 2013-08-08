module Fingerfood.OrderedSequence
( OrderedSequence
, orderedSequence
, partition
, insert
, deleteAll
, merge
) where

import Data.Monoid

import FingerTree

data Key a = NoKey | Key a deriving (Eq, Ord)

instance Monoid (Key a) where
    mempty            = NoKey
    k `mappend` NoKey = k
    _ `mappend` k     = k

newtype Elem a = Elem{ getElem :: a }

newtype OrderedSequence a = OrderedSequence (FingerTree (Key a) (Elem a))

instance Measured (Elem a) (Key a) where
    norm (Elem a) = Key a

orderedSequence :: (Ord a) => [a] -> OrderedSequence a
orderedSequence xs = OrderedSequence $ toTree (map Elem xs)

partition :: (Ord a) => a -> OrderedSequence a -> (OrderedSequence a, OrderedSequence a)
partition k (OrderedSequence xs) = (OrderedSequence l, OrderedSequence r)
    where (l, r) = split (>= Key k) xs

insert :: (Ord a) => a -> OrderedSequence a -> OrderedSequence a
insert x (OrderedSequence xs) = OrderedSequence $ l >< (Elem x <| r)
    where (l, r) = split (>= Key x) xs

deleteAll :: (Ord a) => a -> OrderedSequence a -> OrderedSequence a
deleteAll x (OrderedSequence xs) = OrderedSequence $ l >< r'
    where (l, r)  = split (>= Key x) xs
          (_, r') = split (> Key x) r

merge :: (Ord a) => OrderedSequence a -> OrderedSequence a -> OrderedSequence a
merge (OrderedSequence xs) (OrderedSequence ys) = OrderedSequence $ merge' xs ys
    where merge' as bs = case viewL bs of
            NilL -> as
            ConsL a bs' -> l >< (a <| merge' bs' r)
                where (l, r) = split (> norm a) as

