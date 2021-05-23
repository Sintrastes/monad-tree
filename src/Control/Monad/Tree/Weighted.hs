{-|
Module      : Control.Monad.Tree.Weighted
Description : Utilities for working with weighted trees.
Copyright   : (c) Nathan Bedell, 2021
License     : MIT
Maintainer  : nbedell@tulane.edu

This module builds off the @Tree@ monad defined in @Control.Monad.Tree@ by defining a branching functor
 @Weighted c f@, where @c@ is a "Cost" type, and @f@ is the base functor (such as @[]@).
 
Using such a weighted tree allows one to use a larger variety of search algorithms, such as @bestFirst@.

When @c@ is @Prob@, @WeightedTree l Prob f@ can be used to emulate probabalistic logic programming
 by using the relational DSL provided by this module.

_-}
module Control.Monad.Tree.Weighted (
  -- ** Weighted search tree definitions
  Weighted(..),
  WeightedNode(..),
  WeightedTree,
  -- ** Search algorithms
  bestFirst',
  bestFirst,
  -- ** Relational DSL
  Prob,
  labeledFacts
  
) where

import Control.Monad.Tree
import Control.Applicative
import Data.List
import Data.Functor.Classes


-- | Pair whose Ord instance only relies on the ord instance
--   of @c@.
data WeightedNode c x = WeightedNode c x deriving(Eq)

instance (Eq x, Ord c) => Ord (WeightedNode c x) where
  compare (WeightedNode x _) (WeightedNode y _) = compare x y

-- | Newtype functor used to define a @WeightedTree@.
newtype Weighted c f x = Weighted (f (WeightedNode c x))

instance Functor f => Functor (Weighted c f) where
  fmap f (Weighted xs) = Weighted $ fmap (\(WeightedNode c x) -> (WeightedNode c (f x))) xs 

instance (Functor f, Eq c, Eq1 f, Eq (f c)) => Eq1 (Weighted c f) where 
  liftEq eq (Weighted xs) (Weighted ys) 
    = (liftEq eq 
        ((\(WeightedNode c x) -> x) <$> xs) 
        ((\(WeightedNode c x) -> x) <$> ys)) &&
      ((\(WeightedNode c x) -> c) <$> xs) == ((\(WeightedNode c x) -> c) <$> ys) 

-- | A rose tree with a cost @c@ associated with each branch.
type WeightedTree l c f a = Tree l (Weighted c f) a

newtype Prob = Prob Double

instance Semigroup Prob where
  (Prob x) <> (Prob y) = Prob $ x * y

instance Monoid Prob where
  mempty = Prob 1.0

instance (Monoid c, Functor f, Applicative f) => Applicative (Weighted c f) where
  pure x = Weighted $ pure $ WeightedNode mempty x 
  fs <*> xs = undefined

instance (Functor f, Alternative f) => Alternative (Weighted Prob f) where
  empty = Weighted $ empty
  (Weighted xs) <|> (Weighted ys) = Weighted $ xs <|> ys

-- | Embed a list of labeled facts into a tree.
labeledFacts :: Functor f => f (c,a) -> WeightedTree () c f a
labeledFacts xs = Node () $ Weighted $ (\(l, x) -> (WeightedNode l (Leaf x))) <$> xs

-- | Implementation of a simple best first search traversal of a tree,
--   with a predicate on labels used to prune the search space.
bestFirst' :: (Eq a, Eq l, Ord c) => (c -> l -> Bool) -> WeightedTree l c [] a -> [a]
bestFirst' p (Leaf x) = [x]
bestFirst' p (Node label (Weighted st')) = [s | (WeightedNode c t) <- st, s <- bestFirst' p t, pTree c t]
  where 
    st = sort st'
    pTree c (Leaf x) = True
    pTree c (Node l _) = p c l
  
-- | Implementation of a simple best first search traversal of a tree.
--   
--   Example:
--
--     >
--     > weightedGraph = labeledFacts [(1,2,5.0), (2,3,1.0), (1,3,4.0)]
--     >
--     > paths :: [(a,a,c)] -> WeightedTree [(a,a,c)] c [] [(a,a,c)]
--     >   
--
--     >>> bestFirst $ paths weightedGraph
--     [[(1,3,4.0)], [(1,2,5.0),(2,3,1.0)]]  
--
bestFirst :: (Eq a, Eq l, Ord c) => WeightedTree l c [] a -> [a]
bestFirst = bestFirst' (\x -> \y -> True)

-- | Implementation of a heuristic breadth first search algorithm.
heuristicBfs :: (Traversable f, Ord c, Ord d) => (l -> c -> d) -> WeightedTree l c f a -> f a
heuristicBfs = undefined
