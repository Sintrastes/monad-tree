{-# LANGUAGE UndecidableInstances #-}

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
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Error.Class
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Control.Monad.Writer.Class

data TreeM n f m a = 
    Leaf a 
  | Node n (m (f (TreeM n f m a)))

newtype TreeT n f m a = TreeT { runTreeT :: m (TreeM n f m a) }

bind :: (Monad m, Functor f, Traversable f) => TreeT n f m a -> (a -> TreeT n f m b) -> TreeT n f m b 
bind x f = TreeT $ do
    x' <- runTreeT x
    case x' of
        Node l mas -> do
            as <- mas
            return $ Node l 
              (mapM (\x -> runTreeT $ bind (TreeT x) f) 
                 (fmap return as))
        Leaf x -> runTreeT $ f x

instance (Functor f, Functor m) => Functor (TreeM n f m) where
    fmap f (Leaf x) = Leaf $ f x
    fmap f (Node l xs) = Node l (fmap (f <$>) <$> xs)

instance (Functor f, Functor m) => Functor (TreeT n f m) where
    fmap f x = TreeT $ runTreeT x <&> (\x' ->
            case x' of
                Leaf v -> Leaf $ f v
                Node l vs -> Node l (((f <$>) <$>) <$> vs)
        ) 

instance (Monad m, Traversable f) => Applicative (TreeT n f m) where
    pure x = TreeT $ pure $ Leaf x
    (<*>) fs xs = do
        f <- fs
        x <- xs
        pure (f x)

instance (Traversable f, Monad m) => Monad (TreeT n f m) where
    (>>=) x f = bind x f

instance MonadTrans (TreeT n f) where
    lift x = TreeT $ Leaf <$> x

instance (MonadIO m, Traversable f) => MonadIO (TreeT n f m) where
    liftIO x = TreeT $ liftIO $ Leaf <$> x

instance (MonadState s m, Traversable f) => MonadState s (TreeT n f m) where
    get = lift get
    put = lift . put

-- TODO: Not sure how to define these instances.
{-
instance (MonadReader r m, Traversable f) => MonadReader r (TreeT n f m) where
    ask = lift ask
    local = undefined

instance (MonadWriter w m, Traversable f) => MonadWriter w (TreeT n f m) where
    tell   = lift . tell
    listen = undefined
    pass = undefined

instance (MonadError e m, Traversable f) => MonadError e (TreeT n f m) where
    throwError = lift throwError
    catchError = undefined
-}