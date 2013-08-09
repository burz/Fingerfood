module Fingerfood.Grid
( Grid
, Position
, grid
, getPosition
, setPosition
) where

import Fingerfood.Sequence

type    Column a = Sequence a
newtype Grid a   = Grid (Sequence (Column a))

class Position a where
    empty :: a

grid :: (Position a) => Int -> Int -> Grid a
grid x y = Grid . sequence' $ take x $ repeat . sequence' $ take y $ repeat empty

getPosition :: Grid a -> Int -> Int -> Maybe a
getPosition (Grid c) x y = c ! x >>= (! y)

setPosition :: Grid a -> Int -> Int -> a -> Grid a
setPosition (Grid cs) x y p = Grid $ set cs x c
    where Just c = cs ! x >>= \s -> Just $ set s y p

