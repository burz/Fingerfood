module Fingerfood.Grid
( Grid(..)
, Position(..)
, grid
, toGrid
, getList
, getPosition
, setPosition
) where

import Fingerfood.Sequence

class Position a where
    empty :: a

type    Column a = Sequence a
newtype Grid a   = Grid (Sequence (Column a))

instance (Show a) => Show (Grid a) where
    show (Grid s) = foldr1 (\a b -> a ++ '\n' : b) $ map show (getList s)

grid :: (Position a) => Int -> Int -> Grid a
grid x y = Grid . sequence' $ take y $ repeat . sequence' $ take x $ repeat empty

toGrid :: [[a]] -> Grid a
toGrid xss = Grid . sequence' $ map sequence' xss

getPosition :: Grid a -> Int -> Int -> Maybe a
getPosition (Grid c) x y = c ! y >>= (! x)

setPosition :: Grid a -> Int -> Int -> a -> Grid a
setPosition (Grid cs) x y p = Grid $ set cs x c
    where Just c = cs ! y >>= \s -> Just $ set s x p

type    RelativeDimensions = (Int, Int, Int, Int)
newtype Subgrid a          = Subgrid (Sequence (Maybe (Column (Maybe a))))

instance (Show a) => Show (Subgrid a) where
    show (Subgrid s) = foldr1 (\a b -> a ++ '\n' : b) $ map show (getList s)

subgrid :: Grid a -> Int -> Int -> RelativeDimensions -> Subgrid a
subgrid (Grid s) x y (x1, x2, y1, y2) = Subgrid $ subsequence s' (y - y1) (y + y2)
    where s' = sequence' $ map (\s -> subsequence s (x - x1) (x + x2)) (getList s)

