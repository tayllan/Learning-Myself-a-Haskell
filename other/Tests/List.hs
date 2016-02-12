module Tests.List (
	isFunctor,
	isApplicative,
	isMonad
) where

import qualified Laws.Functor as F
import qualified Laws.Applicative as A
import qualified Laws.Monad as M

isFunctor :: Bool
isFunctor = and [
		F.identity ([] :: [Int]),
		F.identity [1..10],
		F.composition (^2) (*2) [],
		F.composition (^2) (*2) [1..10]
	]

isApplicative :: Bool
isApplicative = and [
		A.identity (^2) ([] :: [Int]),
		A.identity (^2) [1..10],
		A.someRule ([] :: [Int]),
		A.someRule [1..10],
		A.composition [(^2)] [(*2)] [],
		A.composition [(^2)] [(*2)] [1..10],
		A.homomorphism (^2) 10,
		A.interchange [(^2)] 10
	]

isMonad :: Bool
isMonad = and [
		M.leftIdentity (:[]) 1,
		M.rightIdentity [1],
		M.rightIdentity ([] :: [Int]),
		M.associativity (:[]) (:[]) ([1]),
		M.associativity (:[]) (:[]) ([] :: [Int])
	]