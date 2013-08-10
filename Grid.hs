module Fingerfood.Grid
( Grid(..)
, Position(..)
, grid
, toGrid
, getPosition
, setPosition
) where

import Fingerfood.Sequence

class Position a where
    empty :: a

type    Column a = Sequence a
newtype Grid a   = Grid (Sequence (Column a))

grid :: (Position a) => Int -> Int -> Grid a
grid x y = Grid . sequence' $ take y $ repeat . sequence' $ take x $ repeat empty

toGrid :: [[a]] -> Grid a
toGrid xss = Grid . sequence' $ map sequence' xss

getPosition :: Grid a -> Int -> Int -> Maybe a
getPosition (Grid c) x y = c ! y >>= (! x)

setPosition :: Grid a -> Int -> Int -> a -> Grid a
setPosition (Grid cs) x y p = Grid $ set cs x c
    where Just c = cs ! y >>= \s -> Just $ set s x p

