import qualified Data.Foldable as F
import Data.Monoid

data Tree a = Empty
			| Node a (Tree a) (Tree a)
			deriving (Show, Read, Eq)

instance F.Foldable Tree where
	foldMap f Empty = mempty
	foldMap f (Node x l r) = F.foldMap f l `mappend`
							f x `mappend`
							F.foldMap f r

-- examples:

-- a = Node 12 (Node 7 Empty Empty) Empty

-- should print "[7, 12]"
-- F.foldMap (\x -> [x]) a

-- should print "84"
-- F.foldl (*) 1 a