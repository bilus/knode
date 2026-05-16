module Knode.Combinators (ifM, andM, whenM) where

ifM :: (Monad m) => m Bool -> m a -> m a -> m a
ifM cond t f = cond >>= \b -> if b then t else f

andM :: (Monad m) => m Bool -> m Bool -> m Bool
andM ma mb = ma >>= \a -> if a then mb else pure False

whenM :: (Monad m) => m Bool -> m () -> m ()
whenM cond action = ifM cond action (pure ())
