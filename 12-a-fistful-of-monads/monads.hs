import Control.Monad
import Data.Char

applyMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
applyMaybe Nothing _= Nothing
applyMaybe (Just x) f = f x

type Birds = Int
type Pole = (Birds, Birds)

landLeft :: Birds -> Pole -> Maybe Pole
landLeft n (left,right)
	| abs ((left + n) - right) < 4 = Just (left + n, right)
	| otherwise = Nothing

landRight :: Birds -> Pole -> Maybe Pole
landRight n (left,right)
	| abs (left - (right + n)) < 4 = Just (left, right + n)
	| otherwise = Nothing

x -: f = f x

foo :: Maybe String
foo = Just 3 >>= (\x ->
	Just "!" >>= (\y ->
	Just (show x ++ y)))

-- or, the DO notation, which is syntic sugar to chaining monadic values:
foo2 :: Maybe String
foo2 = do
	x <- Just 3
	y <- Just "!"
	Just (show x ++ y)

-- using the "do" notation to chain some operations simulating the landing
-- of birds on the Pole.
-- (The Nothing in the middle of it is just to see everything failing)
routine :: Maybe Pole
routine = do
	start <- return (0,0)
	first <- landLeft 2 start
--	Nothing
--	this "Nothing" could be rewritten as:
--	_ <- Nothing
	second <- landRight 2 first
	landLeft 1 second
-- the above is the same as:
routine2 :: Maybe Pole
routine2 = return (0, 0) >>= landLeft 2 >>= landRight 2 >>= landLeft 1
-- and including the "-- Nothing":
-- routine2 = return (0, 0) >>= landLeft 2 >> Nothing >>= landRight 2 >>= landLeft 1

-- the "do" notation allows the usage of pattern matching:
firstUpper :: String -> Maybe Char
firstUpper string = do
	(first:xs) <- Just string
	return $ toUpper first

listOfTuples :: [(Int,Char)]
listOfTuples = do
	n <- [1,2]
	ch <- ['a','b']
	return (n,ch)
-- the above can also be represented as:
listOfTuples2 :: [(Int,Char)]
listOfTuples2 = [1,2] >>= \x -> ['a','b'] >>= \y -> return (x,y)
-- and also as:
listOfTuples3 :: [(Int,Char)]
listOfTuples3 = [(n,ch) | n <- [1,2], ch <- ['a','b']]

-- starting with list comprehensions
sevensOnly :: [Int]
sevensOnly = [x | x <- [1..50], '7' `elem` show x]
-- now using monads + guards
sevensOnly2 :: [Int]
sevensOnly2 = [1..50] >>= (\x -> guard ('7' `elem` show x) >> return x)
-- the above works because "guard" returns a list with an empty tuple if the
-- condition is True: "if X contains 7, return [()]", which represents a successful
-- operation, which is then overwritten by the ">>" operatorn, that just returns X.
-- Otherwise, if the condition is False, the "guard" returns an empty list, which
-- represents a failed operation overall.

-- and finaly with monads and the "do" notation
sevensOnly3 :: [Int]
sevensOnly3 = do
	x <- [1..50]
	guard ('7' `elem` show x)
	return x

type KnightPos = (Int, Int)

moveKnight :: KnightPos -> [KnightPos]
moveKnight (c,r) = do
	-- (c',r') takes on every value from the list of movements
	(c',r') <- [(c+2,r-1),(c+2,r+1),(c-2,r-1),(c-2,r+1),(c+r,1-2),(c+1,r+2),(c-1,r-2),(c-1,r+2)]
	-- guard used on the do notation with the monads "[]" is the same as
	-- filtering on list comprehensions
	guard (c' `elem` [1..8] && r' `elem` [1..8])
	return (c',r')
-- "moveKnight" can be easly rewritten without monads using list comprehension:
moveKnight2 :: KnightPos -> [KnightPos]
moveKnight2 (c,r) = [(c',r') | (c',r') <- moves, c' `elem` [1..8] && r' `elem` [1..8]]
	where moves = [(c+2,r-1),(c+2,r+1),(c-2,r-1),(c-2,r+1),(c+r,1-2),(c+1,r+2),(c-1,r-2),(c-1,r+2)]
-- or the site's version with filter
moveKnight3 :: KnightPos -> [KnightPos]
moveKnight3 (c,r) = filter onBoard [(c+2,r-1),(c+2,r+1),(c-2,r-1),(c-2,r+1),(c+r,1-2),(c+1,r+2),(c-1,r-2),(c-1,r+2)]
	where onBoard (c,r) = c `elem` [1..8] && r `elem` [1..8]

-- now, using the "moveKnight" function I can make 3 consecutive moves
-- starting from a given position
in3 :: KnightPos -> [KnightPos]
in3 start = return start >>= moveKnight >>= moveKnight >>= moveKnight
-- the above can be rewritten without the usage of "return"
in32 :: KnightPos -> [KnightPos]
in32 start = moveKnight start >>= moveKnight >>= moveKnight

-- and finally, I'm able to tell if you can access a given position
-- starting from another given position
canReachIn3 :: KnightPos -> KnightPos -> Bool
canReachIn3 start end = end `elem` in3 start