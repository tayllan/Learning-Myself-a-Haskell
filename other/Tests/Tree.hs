module Tests.Tree (
	Tree(..),
	singleton,
	treeInsert,
	isFunctor,
	isApplicative,
	isMonad
) where

import Control.Applicative
import qualified Laws.Functor as F
import qualified Laws.Applicative as A
import qualified Laws.Monad as M

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

instance Functor Tree where
	fmap f EmptyTree = EmptyTree
	fmap f (Node x leftBranch rightBranch) = Node (f x) (fmap f leftBranch) (fmap f rightBranch)

instance Applicative Tree where
	pure = singleton
	(Node f leftBranch rightBranch) <*> x= fmap f x
	EmptyTree <*> _ = EmptyTree

isFunctor :: Bool
isFunctor = and [
		F.identity (EmptyTree :: Tree Int),
		F.identity $ singleton 1,
		F.composition (^2) (*2) EmptyTree,
		F.composition (^2) (*2) (singleton 10)
	]

isApplicative :: Bool
isApplicative = and [
		A.identity (^2) EmptyTree,
		A.identity (^2) (singleton 10),
		A.someRule (EmptyTree :: Tree Int),
		A.someRule $ singleton 10,
		A.composition (singleton (^2)) (singleton (*2)) EmptyTree,
		A.composition (singleton (^2)) (singleton (*2)) (singleton 10),
		A.homomorphism (^2) 10,
		A.interchange (singleton (^2)) 10
	]

isMonad :: Bool
isMonad = and [
		-- TODO: make Tree an instance of the Monad type class
		--leftIdentity singleton 1,
		--rightIdentity $ singleton 1,
		--rightIdentity (EmptyTree :: Tree Int),
		--associativity singleton singleton (singleton 1),
		--associativity singleton singleton (EmptyTree :: Tree Int)
		False
	]