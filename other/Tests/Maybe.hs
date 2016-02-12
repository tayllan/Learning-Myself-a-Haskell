module Tests.Maybe (
	isFunctor,
	isApplicative,
	isMonad
) where

import qualified Laws.Functor as F
import qualified Laws.Applicative as A
import qualified Laws.Monad as M

isFunctor :: Bool
isFunctor = and [
		F.identity (Nothing :: Maybe Int),
		F.identity $ Just 1,
		F.composition (^2) (*2) Nothing,
		F.composition (^2) (*2) (Just 10)
	]

isApplicative :: Bool
isApplicative = and [
		A.identity (^2) Nothing,
		A.identity (^2) (Just 10),
		A.someRule (Nothing :: Maybe Int),
		A.someRule $ Just 10,
		A.composition (Just (^2)) (Just (*2)) Nothing,
		A.composition (Just (^2)) (Just (*2)) (Just 10),
		A.homomorphism (^2) 10,
		A.interchange (Just (^2)) 10
	]

isMonad :: Bool
isMonad = and [
		M.leftIdentity Just 1,
		M.rightIdentity $ Just 1,
		M.rightIdentity (Nothing :: Maybe Int),
		M.associativity Just Just (Just 1),
		M.associativity Just Just (Nothing :: Maybe Int)
	]