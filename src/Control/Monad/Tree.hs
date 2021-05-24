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
 @b ~ Double@, a best-first or probabilistic search could be used. This idea is implemented
 in @Control.Monad.Tree.Weighted@.
 
While these instances may be used directly to facilitate non-deterministic programming
 similarly to how the list monad is commonly used, this module also provides a simple
 EDSL for typed relational programming in Haskell. 
 
Values of types @Tree l (a,b,c)@ can be viewed as "tabular relations", where any of the arguments
 can be queried. This is facilitated by the @query@ function exposed by this module.
 
For values of type @Tree l Facts@ where @Facts@ is a sum type can also be viewed as general sets of
 facts that can be queried -- where the terms of @Facts@ can be viewed as particular typed relations.
 For instance:
 
 >
 > newtype Person = Person String
 >
 > data Facts =
 >    FriendOf Person Person
 >  | Father Person Person
 >  | Mother Person Person
 >  | Parent Person Person
 > 
 
This module also supplies various helper functions (applyRule, applyRuleL, and variants) to transform such sets of facts by applying prolog-style "rules" to derive further facts given a set of base facts.
-}
module Control.Monad.Tree (
  -- ** Tree data type
  Tree(..),
  -- ** Search algorithms
  dfs,
  dfs',
  bfs,
  bfs',
  -- ** Relational DSL
  facts,
  query,
  applyRule,
  applyRuleL,
  applyRuleL2,
  applyRuleL3,
  applyRuleL4,
  applyRuleL5
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
  
-- dfs with a predicate on the labels
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
             
-- | Embed a list of facts into a tree.
facts :: Functor f => f a -> Tree () f a
facts xs = Node () (Leaf <$> xs)

-- | Query a tree relation by supplying functions for supplying input and
--   output arguments.
--
--   >>> facts = facts [(1,2),(3,4),(5,6),(7,8)]
--   >>> qfs $ query facts (\(x,y) -> x) (\(x,y) -> y) 5
--   [6]
--
query :: (Functor f, Eq a, Alternative (Tree n f)) => Tree n f x -> (x -> a) -> (x -> b) -> a -> Tree n f b
query tree inputArgs outputArgs x =
    tree >>= (\y ->
        if inputArgs y == x
          then return $ outputArgs y
          else empty
    )

-- | Helper function to duplicate a tree
--   Takes a tree returning facts, and returns
--   a tree returning pairs of facts. 
dupTree :: Functor f => Tree n f a -> Tree n f (a,a)
dupTree tree = do
  x <- tree
  y <- tree
  return (x,y) 
  
-- | Helper function to duplicate a tree
--   Takes a tree returning facts, and returns
--   a tree returning triples of facts. 
dupTree3 :: Functor f => Tree n f a -> Tree n f (a,a,a)
dupTree3 tree = do
  x <- tree
  y <- tree
  z <- tree
  return (x,y,z) 
  
-- | Helper function to duplicate a tree
--   Takes a tree returning facts, and returns
--   a tree returning quadruples of facts. 
dupTree4 :: Functor f => Tree n f a -> Tree n f (a,a,a,a)
dupTree4 tree = do
  x <- tree
  y <- tree
  z <- tree
  w <- tree
  return (x,y,z,w) 
  
-- | Helper function to duplicate a tree
--   Takes a tree returning facts, and returns
--   a tree returning quintuples of facts. 
dupTree5 :: Functor f => Tree n f a -> Tree n f (a,a,a,a,a)
dupTree5 tree = do
  x <- tree
  y <- tree
  z <- tree
  w <- tree
  q <- tree
  return (x,y,z,w,q) 
    
-- | Apply a function to a tree deriving new facts
--   from old facts, and merge the trees to obtain
--   a new tree with both the original facts, and the
--   facts derived with the supplied rule.
--
--   Comes in a number of variants (e.x. applyRule2) for working with different arities, for rules
--   that depend on multiple base facts.
--
--   For example, given a relation defining a graph:
--
--   > graphAdjacency = facts [(1,2),(2,3),(3,4)]
--
--   We can apply a rule to obtain the transitive closure of the graph
--
--   > graphAdjacency `applyRule2` (\((x,y), (y',z)) ->
--   >    if y == y'
--   >      then pure (x,z)
--   >      else empty
--   >  )
--
applyRule :: (Functor f, Alternative (Tree n f)) => Tree n f a -> (a -> Tree n f a) -> Tree n f a
applyRule tree deriveFacts = tree <|> (tree >>= deriveFacts)

-- Note: Once there are no more facts to derive, this does not terminate.
-- However, if we use the same form as applyRule above, the rules are only applied once.
-- I also think that a better form for this function might be 
--     Tree n f a -> (a -> f a) -> Tree n f a
-- I think this would also be helpful for the terminination issue, since
-- if the derivation returns empty from the alternative instance, we can stop deriving
-- new results.
-- Also, I think this would work better for probabalistic/weighted search, since
-- we can then just depend on the Alternative instance for f, which would
-- take care of the combination of weights.
applyRule2 :: (Functor f, Alternative (Tree n f)) => Tree n f a -> ((a,a) -> Tree n f a) -> Tree n f a
applyRule2 tree deriveFacts = tree <|> (applyRule2 ((dupTree tree) >>= deriveFacts) deriveFacts)

-- | Apply a function to a tree deriving new facts that transforms the old facts,
--   (for instance, by deriving a new set of facts).
--
--   Comes in a number of variants (applyRuleLn) for working with different arities, for rules
--   that depend on multiple base facts.
--
--   For example:
--
--   > data Person = Person String 
--   >
--   > data PaternalFacts = 
--   >    Father Person Person
--   >    Mother Person Person
--   >
--   > data GrandparentFacts = 
--   >    Grandfather Person Person
--   >    Grandmother Person Person
--   > 
--   > type ExtendedFacts = Either PaternalFacts GrandparentFacts
--   >
--   > lift :: PaternalFacts -> ExtendedFacts
--   > lift = Left
--   > 
--   > deriveGrandparents :: Tree () PaternalFacts -> Tree () ExtendedFacts
--   > deriveGrandparents tree = tree `applyRuleL2 lift` (\facts ->
--   >     case facts of
--   >       (Mother x y, Mother z w) 
--   >          | y == z -> pure $ Grandmother x w
--   >       (Father x y, Mother z w) 
--   >          | y == z -> pure $ Grandmother x w
--   >       (Mother x y, Father z w) 
--   >          | y == z -> pure $ Grandfather x w
--   >       (Father x y, Father z w) 
--   >          | y == z -> pure $ Grandfather x w
--   > )
--   
applyRuleL :: (Functor f, Alternative (Tree n f)) => (a -> b) -> Tree n f a -> (a -> Tree n f b) -> Tree n f b
applyRuleL lift tree deriveFacts 
    = (fmap lift tree) <|> (tree >>= deriveFacts)

-- | Variant of @applyRuleL@ for deriving new facts from two base facts.
applyRuleL2 :: (Functor f, Alternative (Tree n f)) => (a -> b) -> Tree n f a -> ((a,a) -> Tree n f b) -> Tree n f b
applyRuleL2 lift tree deriveFacts
    = (fmap lift tree) <|> ((dupTree tree) >>= deriveFacts)

-- | Variant of @applyRuleL@ for deriving new facts from three base facts.
applyRuleL3 :: (Functor f, Alternative (Tree n f)) => (a -> b) -> Tree n f a -> ((a,a,a) -> Tree n f b) -> Tree n f b
applyRuleL3 lift tree deriveFacts
    = (fmap lift tree) <|> ((dupTree3 tree) >>= deriveFacts)

-- | Variant of @applyRuleL@ for deriving new facts from four base facts.
applyRuleL4 :: (Functor f, Alternative (Tree n f)) => (a -> b) -> Tree n f a -> ((a,a,a,a) -> Tree n f b) -> Tree n f b
applyRuleL4 lift tree deriveFacts
    = (fmap lift tree) <|> ((dupTree4 tree) >>= deriveFacts)

-- | Variant of @applyRuleL@ for deriving new facts from five base facts.
applyRuleL5 :: (Functor f, Alternative (Tree n f)) => (a -> b) -> Tree n f a -> ((a,a,a,a,a) -> Tree n f b) -> Tree n f b
applyRuleL5 lift tree deriveFacts
    = (fmap lift tree) <|> ((dupTree5 tree) >>= deriveFacts)


