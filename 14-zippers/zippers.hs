-- a Zipper is a data structure that cointains a focused part of another
-- data structure and its surroundings, in a way that's possible to
-- reconstruct all of it.

x -: f = f x

data Tree a = Empty
			| Node a (Tree a) (Tree a)
			deriving (Show, Eq)
data Crumb a = LeftCrumb a (Tree a)
				| RightCrumb a (Tree a)
				deriving (Show)
type Breadcrumbs a = [Crumb a]
type TreeZipper a = (Tree a, Breadcrumbs a)

goLeft :: TreeZipper a -> TreeZipper a
goLeft (Node x l r, bs) = (l, LeftCrumb x r:bs)
goLeft (Empty, bs) = (Empty, bs)

goRight :: TreeZipper a -> TreeZipper a
goRight (Node x l r, bs) = (r, RightCrumb x l:bs)
goRight (Empty, bs) = (Empty, bs)

goUp :: TreeZipper a -> TreeZipper a
goUp (t, LeftCrumb x r:bs) = (Node x t r, bs)
goUp (t, RightCrumb x l:bs) = (Node x l t, bs)

modify :: (a -> a) -> TreeZipper a -> TreeZipper a
modify f (Node x l r, bs) = (Node (f x) l r, bs)
modify f (Empty, bs) = (Empty, bs)

attach :: Tree a -> TreeZipper a -> TreeZipper a
attach t (_, bs) = (t, bs)

topMost :: TreeZipper a -> TreeZipper a
topMost (t,[]) = (t,[])
topMost z = topMost (goUp z)

type ListZipper a = ([a],[a])

goForward :: ListZipper a -> ListZipper a
goForward (x:xs, bs) = (xs, x:bs)

goBackward :: ListZipper a -> ListZipper a
goBackward (xs, b:bs) = (b:xs, bs)