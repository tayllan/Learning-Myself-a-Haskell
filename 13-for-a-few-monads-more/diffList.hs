import Data.Monoid
import Control.Monad.Writer

-- creating a Difference List
newtype DiffList a = DiffList {
	getDiffList :: [a] -> [a]
}

toDiffList :: [a] -> DiffList a
toDiffList xs = DiffList (xs++)

fromDiffList :: DiffList a -> [a]
fromDiffList (DiffList f) = f []

instance Monoid (DiffList a) where
	mempty = DiffList (\xs -> [] ++ xs)
	(DiffList f) `mappend` (DiffList g) = DiffList (\xs -> f (g xs))

-- using it:
-- fromDiffList (toDiffList [1,2,3,4] `mappend` toDiffList [1,2,3])
-- prints: [1,2,3,4,1,2,3]