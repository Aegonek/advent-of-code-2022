module Control.Extra.Monad where

import Control.Monad

infixl 1 <<

-- | Sequence two monadic computations left to right, discarding the result of the second computation.
(<<) :: Monad m => m a -> m b -> m a
x << y = do
  x' <- x
  _ <- y
  return x'