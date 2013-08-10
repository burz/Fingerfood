module Fingerfood.FingerTree
( toList
, Measured(..)
, FingerTree(..)
, toTree
, (<|)
, (|>)
, ViewL(..)
, viewL
, (><)
, Split(..)
, splitTree
, split
, takeUntil
, dropUntil
) where

import Data.Monoid

class Reduce f where
    reducer :: (a -> b -> b) -> (f a -> b -> b)
    reducel :: (b -> a -> b) -> (b -> f a -> b)

instance Reduce [] where
    reducer (-<) x z = foldr (-<) z x
    reducel (>-) x z = foldl (>-) x z

toList :: (Reduce f) => f a -> [a]
toList s = cat s [] where cat = reducer (:)

class (Monoid v) => Measured a v where
    norm :: a -> v

data Node v a = Node2 v a a | Node3 v a a a

node2 :: (Measured a v) => a -> a -> Node v a
node2 a b = Node2 (norm a <> norm b) a b

node3 :: (Measured a v) => a -> a -> a -> Node v a
node3 a b c = Node3 (norm a <> norm b <> norm c) a b c

instance (Monoid v) => (Measured (Node v a) v) where
    norm (Node2 v _ _)   = v
    norm (Node3 v _ _ _) = v

instance (Measured a v) => Measured (Digit a) v where
    norm xs = reducel (\i a -> i <> norm a) mempty xs

type Digit a = [a]

data FingerTree v a = Empty
                    | Single a
                    | Deep v (Digit a) (FingerTree v (Node v a)) (Digit a)

deep :: (Measured a v) => Digit a -> FingerTree v (Node v a) -> Digit a -> FingerTree v a
deep pr m sf = Deep (norm pr <> norm m <> norm sf) pr m sf

instance Reduce (Node v) where
    reducer (-<) (Node2 _ a b) z   = a -< (b -< z)
    reducer (-<) (Node3 _ a b c) z = a -< (b -< (c -< z))

    reducel (>-) z (Node2 _ b a)   = (z >- b) >- a
    reducel (>-) z (Node3 _ c b a) = ((z >- c) >- b) >- a

instance Reduce (FingerTree v) where
    reducer (-<) Empty z            = z
    reducer (-<) (Single x) z       = x -< z
    reducer (-<) (Deep _ pr m sf) z = pr *< (m **< (sf *< z))
        where (*<) = reducer (-<)
              (**<) = reducer (reducer (-<))

    reducel (>-) z Empty            = z
    reducel (>-) z (Single x)       = z >- x
    reducel (>-) z (Deep _ pr m sf) = ((z >* pr) >** m) >* sf
        where (>*) = reducel (>-)
              (>**) = reducel (reducel (>-))

instance (Measured a v) => Measured (FingerTree v a) v where
    norm Empty          = mempty
    norm (Single x)     = norm x
    norm (Deep v _ _ _) = v

infixr 5 <|
(<|) :: (Measured a v) => a -> FingerTree v a -> FingerTree v a
a <| Empty                    = Single a
a <| Single b                 = deep [a] Empty [b]
a <| Deep _ [b, c, d, e] m sf = deep [a, b] (node3 c d e <| m) sf
a <| Deep _ pr m sf           = deep ([a] ++ pr) m sf

infixr 5 |>
(|>) :: (Measured a v) => FingerTree v a -> a -> FingerTree v a
Empty |> b                    = Single b
Single a |> b                 = deep [a] Empty [b]
Deep _ pr m [a, b, c, d] |> e = deep pr (m |> node3 a b c) [c, d]
Deep _ pr m sf |> a           = deep pr m (sf ++ [a])

(*<|) :: (Reduce f, Measured a v) => f a -> FingerTree v a -> FingerTree v a
(*<|) = reducer (<|)

(|>*) :: (Reduce f, Measured a v) => FingerTree v a -> f a -> FingerTree v a
(|>*) = reducel (|>)

toTree :: (Reduce f, Measured a v) => f a -> FingerTree v a
toTree s = s *<| Empty

data ViewL s a = NilL | ConsL a (s a)
data ViewR s a = NilR | ConsR (s a) a

viewL :: (Measured a v) => FingerTree v a -> ViewL (FingerTree v) a
viewL Empty            = NilL
viewL (Single x)       = ConsL x Empty
viewL (Deep _ pr m sf) = ConsL (head pr) (deepL (tail pr) m sf)

viewR :: (Measured a v) => FingerTree v a -> ViewR (FingerTree v) a
viewR Empty = NilR
viewR (Single x) = ConsR Empty x
viewR (Deep _ pr m sf) = ConsR (deepR pr m (init sf)) (last pr)

deepL :: (Measured a v) => [a] -> FingerTree v (Node v a) -> Digit a -> FingerTree v a
deepL [] m sf =
    case viewL m of
        NilL       -> toTree sf
        ConsL a m' -> deep (toList a) m' sf
deepL pr m sf = deep pr m sf

deepR :: (Measured a v) => Digit a -> FingerTree v (Node v a) -> [a] -> FingerTree v a
deepR pr m [] =
    case viewR m of
        NilR       -> toTree pr
        ConsR m' a -> deep pr m' (toList a)
deepR pr m sf = deep pr m sf

isEmpty :: (Measured a v) => FingerTree v a -> Bool
isEmpty x =
    case viewL x of
        NilL      -> True
        ConsL _ _ -> False

headL :: (Measured a v) => FingerTree v a -> a
headL x = case viewL x of ConsL a _ -> a

tailL :: (Measured a v) => FingerTree v a -> FingerTree v a
tailL x = case viewL x of ConsL _ x' -> x'

nodes :: (Measured a v) => [a] -> [Node v a]
nodes [a, b]       = [node2 a b]
nodes [a, b, c]    = [node3 a b c]
nodes [a, b, c, d] = [node2 a b, node2 c d]
nodes (a:b:c:xs)   = node3 a b c : nodes xs

app3 :: (Measured a v) => FingerTree v a -> [a] -> FingerTree v a -> FingerTree v a
app3 Empty ts xs      = ts *<| xs
app3 xs ts Empty      = xs |>* ts
app3 (Single x) ts xs = x <| (ts *<| xs)
app3 xs ts (Single x) = (xs |>* ts) |> x
app3 (Deep _ pr1 m1 sf1) ts (Deep _ pr2 m2 sf2)
    = deep pr1 (app3 m1 (nodes (sf1 ++ ts ++ pr2)) m2) sf2

(><) :: (Measured a v) => FingerTree v a -> FingerTree v a -> FingerTree v a
xs >< ys = app3 xs [] ys

data Split f a = Split (f a) a (f a)

splitDigit :: (Measured a v) => (v -> Bool) -> v -> Digit a -> Split [] a
splitDigit p i [a] = Split [] a []
splitDigit p i (a:as)
    | p i' = Split [] a as
    | otherwise = let Split l x r = splitDigit p i' as in Split (a:l) x r
    where i' = i <> norm a

splitTree :: (Measured a v) => (v -> Bool) -> v -> FingerTree v a -> Split (FingerTree v) a
splitTree p i (Single x) = Split Empty x Empty
splitTree p i (Deep _ pr m sf)
    | p vpr     = let Split l x r    = splitDigit p i pr
                  in  Split (toTree l) x (deepL r m sf)
    | p vm      = let Split ml xs mr = splitTree p vpr m
                      Split l x r    = splitDigit p (vpr <> norm ml) (toList xs)
                  in  Split (deepR pr ml l) x (deepL r mr sf)
    | otherwise = let Split l x r    = splitDigit p vm sf
                  in Split (deepR pr m l) x (toTree r)
    where vpr = i <> norm pr
          vm  = vpr <> norm m

        
split :: (Measured a v) => (v -> Bool) -> FingerTree v a -> (FingerTree v a, FingerTree v a)
split p Empty     = (Empty, Empty)
split p xs
    | p $ norm xs = (l, x <| r)
    | otherwise   = (xs, Empty)
    where Split l x r = splitTree p mempty xs

takeUntil :: (Measured a v) => (v -> Bool) -> FingerTree v a -> FingerTree v a
takeUntil p = fst . split p

dropUntil :: (Measured a v) => (v -> Bool) -> FingerTree v a -> FingerTree v a
dropUntil p = snd . split p

