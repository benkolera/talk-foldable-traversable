{-# LANGUAGE DeriveFunctor #-}
module Examples where

import Prelude hiding (mapM_)
import Control.Applicative
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
-- Sum 1 <> Sum 2 <> mempty <> Sum 3 <> Sum 4
-- Sum 10

example2 = mapM_ print exampleTree1
-- Prints 1 then 2 then 3 then 4 all on their own lines.

-- |
-- >>> example3
-- "abc"
example3 = fold (Node (Leaf "a") "b" (Leaf "c"))

-- |
-- >>> example4
-- Just 2
example4 = find even exampleTree1

-- |
-- >>> example5
-- [1,2,3,4]
example5 = toList exampleTree1

instance Traversable Tree where
-- traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
   traverse f Empty        = pure Empty
   traverse f (Leaf x)     = Leaf <$> f x
   traverse f (Node l k r) = Node <$> traverse f l <*> f k <*> traverse f r

-- |
-- >>> example6
-- Node (Leaf "I'm from A") "I'm from B" Empty
example6 :: IO (Tree String)
example6 = traverse readFile (Node (Leaf "fileA") "fileB" Empty)

-- Node
--  <$> traverse readFile (Leaf "fileA")
--  <*> readFile "fileB"
--  <*> traverse readFile Empty

-- Node
--   <$> (Leaf <$> readFile "fileA")
--   <*> readFile "fileB"
--   <*> pure Empty

newtype Id a = Id { getId :: a } deriving (Functor)
instance Applicative Id where
  pure = Id
  Id f <*> Id x = Id (f x)
-- |
-- >>> fmap' (*2) exampleTree1
-- Node (Leaf 2) 4 (Node Empty 6 (Leaf 8))
fmap' :: Traversable t => (a -> b) -> t a -> t b
fmap' f = getId . traverse (Id . f)

-- |
-- >>> foldMap' Sum exampleTree1
-- Sum {getSum = 10}
foldMap' :: (Traversable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f = getConst . traverse (Const . f)
