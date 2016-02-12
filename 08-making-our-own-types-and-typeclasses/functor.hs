-- attaching the Tree data type from recursiveDataTypes.hs
data Tree a = EmptyTree
			| Node a (Tree a) (Tree a)
			deriving (Show, Read, Eq)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
	| x == a = Node x left right
	| x < a = Node a (treeInsert x left) right
	| x > a = Node a left (treeInsert x right)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem _ EmptyTree = False
treeElem x (Node a left right)
	| x == a = True
	| x < a = treeElem x left
	| x > a = treeElem x right
-- END

instance Functor Tree where
	fmap f EmptyTree = EmptyTree
	fmap f (Node x leftsub rightsub) = Node (f x) (fmap f leftsub) (fmap f rightsub)


data MyEither a b = MyLeft a
					| MyRight b
					deriving (Show)

instance Functor (MyEither a) where
	fmap f (MyRight x) = MyRight (f x)
	fmap f (MyLeft x) = MyLeft x

testing :: Bool -> MyEither String Int
testing True = MyRight 12
testing False = MyLeft "left"