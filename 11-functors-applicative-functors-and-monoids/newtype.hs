newtype CharList = CharList {
	getCharList :: [Char]
} deriving (Eq, Show)

-- if you do something like "fmap (+2) (1, 1)" the result is "(1, 3)",
-- because to make "(,)" an instance of Functor, it must be partially applied.

-- if you want to make so that the first element is the one that gets
-- mapped, do something like this:
newtype Pair b a = Pair {
	getPair :: (a,b)
}

-- and now make Pair an instance of the Functor typeclass, mapping the first element
instance Functor (Pair c) where
	fmap f (Pair (x, y)) = Pair (f x, y)

-- and to use it:
-- getPair $ fmap (+2) (Pair (1, 1))