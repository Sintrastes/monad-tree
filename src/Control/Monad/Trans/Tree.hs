{-|
Module      : Control.Monad.Tree
Description : Implementation of a non-deterministic tree monad.
Copyright   : (c) Nathan Bedell, 2021
License     : MIT
Maintainer  : nbedell@tulane.edu

This module contains the definition of a monad transformer for monadic trees.

Note that this implementation is still experimental, in the sense that the monad laws for
 these instances have _not_ been formally proven. Thus, this module should be used with caution.
-}

module Control.Monad.Trans.Tree where

import Data.Functor

data TreeM m n f a = 
    Leaf a 
  | Node n (m (f (TreeM m n f a)))

newtype TreeT m n f a = TreeT { runTreeT :: m (TreeM m n f a) }

bind :: (Monad m, Functor f, Traversable f) => TreeT m n f a -> (a -> TreeT m n f b) -> TreeT m n f b 
bind x f = TreeT $ do
    x' <- runTreeT x
    case x' of
        Node l mas -> do
            as <- mas
            return $ Node l 
              (mapM (\x -> runTreeT $ bind (TreeT x) f) 
                 (fmap return as))
        Leaf x -> runTreeT $ f x

instance (Functor f, Functor m) => Functor (TreeM m n f) where
    fmap f (Leaf x) = Leaf $ f x
    fmap f (Node l xs) = Node l (fmap (f <$>) <$> xs)

instance (Functor f, Functor m) => Functor (TreeT m n f) where
    fmap f x = TreeT $ runTreeT x <&> (\x' ->
            case x' of
                Leaf v -> Leaf $ f v
                Node l vs -> Node l (((f <$>) <$>) <$> vs)
        ) 

instance (Monad m, Traversable f) => Applicative (TreeT m n f) where
    pure x = TreeT $ pure $ Leaf x
    (<*>) fs xs = do
        f <- fs
        x <- xs
        pure (f x)

instance (Traversable f, Monad m) => Monad (TreeT m n f) where
    (>>=) x f = bind x f