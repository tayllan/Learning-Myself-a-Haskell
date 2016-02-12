module Laws.Applicative (
	identity,
	someRule,
	composition,
	homomorphism,
	interchange
) where

import Control.Applicative

identity :: (Eq (f b), Applicative f) => (a -> b) -> f a -> Bool
identity f x = (f <$> x) == (fmap f x)

someRule :: (Eq (f a), Applicative f) => f a -> Bool
someRule x = (id <$> x) == (x)

composition :: (Eq (f c), Applicative f) => f (b -> c) -> f (a -> b) -> f a -> Bool
composition f g x = ((.) <$> f <*> g <*> x) == (f <*> (g <*> x))

-- TODO: implement "homomorphism" properly
-- LAW : pure f <*> pure x = pure (f x)
homomorphism :: (a -> b) -> a -> Bool
homomorphism _ _ = True

interchange :: (Eq (f b), Applicative f) => f (a -> b) -> a -> Bool
interchange f x = (f <*> pure x) == (pure ($ x) <*> f)