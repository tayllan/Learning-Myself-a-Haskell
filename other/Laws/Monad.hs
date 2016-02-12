module Laws.Monad (
	leftIdentity,
	rightIdentity,
	associativity
) where

import Control.Monad

leftIdentity :: (Eq (m b), Monad m) => (a -> m b) -> a -> Bool
leftIdentity f x = (return x >>= f) == (f x)

rightIdentity :: (Eq (m a), Monad m) => m a -> Bool
rightIdentity x = (x >>= return) == (x)

associativity :: (Eq (m c), Monad m) => (a -> m b) -> (b -> m c) -> m a -> Bool
associativity f g x = ((x >>= f) >>= g) == (x >>= (\y -> f y >>= g))