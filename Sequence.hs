module Sequence
( Sequence
, toSequence
, (!)
) where

import Data.Monoid

import FingerTree

newtype Size = Size{ getSize :: Int } deriving (Eq, Ord)

instance Monoid Size where
    mempty = Size 0
    Size m `mappend` Size n = Size (m + n)

newtype Elem a = Elem{ getElem :: a }

newtype Sequence a = Sequence (FingerTree Size (Elem a))

instance Measured (Elem a) Size where
    norm (Elem _) = Size 1

toSequence :: [a] -> Sequence a
toSequence xs = Sequence $ toTree (map Elem xs)

length :: Sequence a -> Int
length (Sequence xs) = getSize $ norm xs

splitAt :: Int -> Sequence a -> (Sequence a, Sequence a)
splitAt i (Sequence xs) = (Sequence l, Sequence r)
    where (l, r) = split (Size i <) xs

(!) :: Sequence a -> Int -> a
Sequence xs ! i = getElem x
    where Split _ x _ = splitTree (Size i <) (Size 0) xs

