-- Code from Section 5 of John Hughes' classic "Why Functional Programming Matters"
-- Modernised to modern Haskell by Ken Friis Larsen, 2017.

module Minmax where


import Data.List (maximumBy)
import Data.Ord (comparing)

data Tree n = Node n [Tree n]
    deriving (Eq, Show, Read)

instance Functor Tree where
  fmap f (Node a subs) = Node (f a) (map (fmap f) subs)



reptree :: (a -> [a]) -> a -> Tree a
reptree f a = Node a (map (reptree f) (f a))

-- Find value for max key from assoc list
maxkey :: Ord k => [(k, v)] -> v
maxkey = snd . maximumBy(comparing fst)

gametree :: (a -> [a]) -> a -> Tree a
gametree = reptree

maximize :: Ord a => Tree a -> a
maximize (Node n []) = n
maximize (Node n sub) = maximum (map minimize sub)

minimize :: Ord a => Tree a -> a
minimize (Node n []) = n
minimize (Node n sub) = minimum (map maximize sub)

-- dynamic evaluator
evaluate :: (a -> [a]) -> (a -> Int) -> a ->  Int
evaluate moves static = maximize . fmap static . gametree moves

evaluatez :: (a -> [a]) -> (a -> Int) -> a ->  Int
evaluatez moves static = maximize . fmap static . prune 3 . gametree moves

aimove :: Int -> (a -> [a]) -> (a -> Int) -> a -> a
aimove depth moves static p = maxkey [(evaluate' move, move) | move <- moves p]
  where evaluate' = minimize . fmap static . prune depth . gametree moves

prune :: Int -> Tree a -> Tree a
prune 0 (Node a _) = Node a []
prune n (Node a sub) = Node a (map (prune (n-1)) sub)
