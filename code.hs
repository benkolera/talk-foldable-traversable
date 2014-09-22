{-# LANGUAGE DeriveFunctor #-}
module Main where

import Prelude hiding (mapM_)
import Data.Foldable
import Data.Monoid
import Data.Traversable

data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a)
  deriving (Show,Functor)

instance Foldable Tree where
-- foldMap :: (Monoid m) => (a -> m) -> Tree a -> m
   foldMap f Empty        = mempty
   foldMap f (Leaf x)     = f x
   foldMap f (Node l k r) = foldMap f l <> f k <> foldMap f r

exampleTree1 :: Tree Integer
exampleTree1 = (Node (Leaf 1) 2 (Node Empty 3 (Leaf 4)))

-- |
-- >>> example1
-- Sum {getSum = 10}
example1 = foldMap Sum exampleTree1
-- foldMap Sum (Leaf 1) <> Sum 2 <> foldMap Sum (Node Empty 3 (Leaf 4))
-- Sum 1 <> Sum 2 <> mempty <> Sum 3 <> foldMap Sum (Leaf 4)
-- Sum 1 <> Sum 2 <> mempty <> Sum 4 <> Sum 4
-- Sum 10

example2 = mapM_ print exampleTree1
-- Prints 1 then 2 then 3 then 4 all on their own lines.

-- |
-- >> example3
-- "abc"
example3 = fold (Node (Leaf "a") "b" (Leaf "c"))

-- |
-- >> example4
-- Just 2
example4 = find even exampleTree1

-- |
-- >> example5
-- [1,2,3,4]
example5 = toList exampleTree1
