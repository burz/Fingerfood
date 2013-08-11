module Fingerfood.Sequence
( Sequence
, sequence'
, length'
, splitAt'
, (!)
, before
, after
, set
, getList
, subsequence
) where

import Data.Monoid

import Fingerfood.FingerTree

newtype Size = Size{ getSize :: Int } deriving (Eq, Ord)

instance Monoid Size where
    mempty = Size 0
    Size m `mappend` Size n = Size (m + n)

newtype Elem a = Elem{ getElem :: a }

newtype Sequence a = Sequence (FingerTree Size (Elem a))

instance Measured (Elem a) Size where
    norm (Elem _) = Size 1

instance (Show a) => Show (Sequence a) where
    show s = show $ getList s

sequence' :: [a] -> Sequence a
sequence' xs = Sequence $ toTree (map Elem xs)

length' :: Sequence a -> Int
length' (Sequence xs) = getSize $ norm xs

splitAt' :: Int -> Sequence a -> (Sequence a, Sequence a)
splitAt' i (Sequence xs) = (Sequence l, Sequence r)
    where (l, r) = split (Size i <) xs

(!) :: Sequence a -> Int -> Maybe a
Sequence xs ! i
    | i < 0 = Nothing
    | i >= length' (Sequence xs) = Nothing
    | otherwise = Just $ getElem x
    where Split _ x _ = splitTree (Size i <) (Size 0) xs

before :: Sequence a -> Int -> Sequence a
before (Sequence t) n = Sequence $ takeUntil (> Size n) t

after :: Sequence a -> Int -> Sequence a
after (Sequence t) n = Sequence $ dropUntil (> Size (n + 1)) t

set :: Sequence a -> Int -> a -> Sequence a
set s n p = Sequence $ (b |> Elem p) >< a
    where (Sequence b) = before s n
          (Sequence a) = after s n

getList :: Sequence a -> [a]
getList (Sequence f) = map (\(Elem x) -> x) (toList f)

subsequence :: Sequence a -> Int -> Int -> Sequence (Maybe a)
subsequence s x1 x2 = sequence' $ map (s !) [x1..x2]

