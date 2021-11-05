{-|
Module      : Control.Monad.Tree
Description : Implementation of a non-deterministic tree monad.
Copyright   : (c) Nathan Bedell, 2021
License     : MIT
Maintainer  : nbedell@tulane.edu

This module contains the definition of a simple rose-tree-like datatype, parameterized by
 both the type of the labels for the branches, the type of elements contained
 in the leaves of the datatype, and the type @f@ of containers used for the branching: @Tree b f a@. 
 
Fixing the type of labels (usually @ b ~ () @ for simplicity), we have instances for
 Functor, Applicative, Monad, and Alternative. This is similar to the list monad, but because
 the underlying datastructure is a tree, and we can represent branchind non-determinism,
 there is more flexibility -- as when trying to extract data from a @Tree@, one can choose
 a search strategy for flattening the tree.
 
This module provides a simple breadth-first-search strategy @bfs@, and a depth-first
 strategy @dfs@, however, theoretically other strategies could be used, taking advantage
 of the information provided by the type of labels `b` for the tree. For instance, with
 @b ~ Double@, a best-first or probabilistic search could be used.
-}
module Control.Monad.Tree (
  -- ** Tree data type
  Tree(..),
  -- ** Search algorithms
  dfs,
  dfs',
  bfs,
  bfs'
) where

import Control.Applicative
import Data.Functor.Classes

-- | Simple rose-tree data structure, with labels of 
--   type @n@ for nodel, labels of types @b@ for branches, containers of type @f@
--   used to branch on tree nodes, and values of type @a@ at the leaves.
--
--   @f@ will usually be @[]@, but this was kept generic to allow for
--   the use of more efficent containers if relevant.
--
data Tree n f a = 
    Leaf a 
  | Node n (f (Tree n f a)) 
  
instance (Eq a, Eq n, Eq1 f) => Eq (Tree n f a) where
  (Leaf x) == (Leaf y) = x == y
  (Node x xs) == (Node y ys) = x == y && liftEq (==) xs ys

instance Functor f => Functor (Tree n f) where
   fmap f (Leaf x) = Leaf (f x)
   fmap f (Node x ys) = Node x $ (f <$>) <$> ys
 
-- Note: This does not take weights into account.
bind :: Functor f => Tree n f a -> (a -> Tree n f b) -> Tree n f b
bind (Leaf x) f = f x
bind (Node x ys) f = Node x $ (\x -> bind x f) <$> ys

instance Functor f => Applicative (Tree n f) where
  pure = Leaf
  (<*>) fs xs = 
      bind xs (\x -> 
      bind fs (\f ->
        pure $ f x)
      )

instance Functor f => Monad (Tree n f) where
   return = pure
   (>>=)  = bind

instance Alternative f => Alternative (Tree () f) where
  empty   = Node () empty
  t <|> s = Node () $ (pure t) <|> (pure s)
  
-- | dfs with a predicate on the labels
dfs' :: (n -> Bool) ->  Tree n [] a -> [a]
dfs' p (Leaf x) = [x]
dfs' p (Node label st) = [s | t <- st, s <- dfs' p t, pTree t]
  where 
    pTree (Leaf x) = True
    pTree (Node l _) = p l
  
-- | Depth first search of a tree
dfs :: Tree n [] a -> [a]
dfs (Leaf x) = [x]
dfs (Node _ st) = [ s | t <- st, s <- dfs t]

-- | Breadth first search with a predicate on the labels.
bfs' :: (n -> Bool) -> Tree n [] a -> [a]
bfs' p t = trav p [t]
  where 
    trav p [] = []
    trav p ((Leaf x) : q) = x : trav p q
    trav p ((Node label st) : q) 
      | p label   = trav p (q ++ st)
      | otherwise = trav p q
             
-- | Breadth first search algorithm for trees
bfs :: Tree n [] a -> [a]
bfs = bfs' (\x -> True)

