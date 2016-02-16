import Data.List (all)
import Data.Ratio

-- "Prob" is going to be used to represent lists of numbers
-- and their associated probabilities of happening
-- e.g. "[(3,1%2),(5,1%4),(9,1%4)]"
newtype Prob a = Prob {
	getProb :: [(a,Rational)]
} deriving Show

-- "fmap" is going to receive a list of "Prob" (like "[(3,1%2),(5,1%4),(9,1%4)]"),
-- and it's going to apply the given function only to the numbers,
-- leaving their probabilities intact. Example:
-- getProb $ fmap negate (Prob [(3,1%2),(5,1%4),(9,1%4)])
-- prints: "[(-3,1 % 2),(-5,1 % 4),(-9,1 % 4)]"
instance Functor Prob where
	fmap f (Prob xs) = Prob $ map (\(x,p) -> (f x,p)) xs

flatten :: Prob (Prob a) -> Prob a
flatten (Prob xs) = Prob $ concat $ map multAll xs
	where multAll (Prob innerxs,p) = map (\(x,r) -> (x,p * r)) innerxs

instance Monad Prob where
	return x = Prob [(x,1 % 1)]
	m >>= f = flatten (fmap f m)
	fail _ = Prob []