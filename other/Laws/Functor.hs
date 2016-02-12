module Laws.Functor (
	identity,
	composition
) where

identity :: (Eq (f x), Functor f) => f x -> Bool
identity x = (fmap id x) == (id x)

composition :: (Eq (f x), Functor f) => (x -> x) -> (x -> x) -> f x -> Bool
composition f g x = (fmap (f . g) x) == (fmap f (fmap g x))