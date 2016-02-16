import Data.Monoid
import Control.Monad.Writer

isBigGang :: Int -> (Bool,String)
isBigGang x = (x > 9, "Compared to a gang of 9 people.")

-- I use pattern matching to take the value out of the tuple context,
-- so that I can pass "x" (which doesn't have a context) to the function
-- "f" that accepts a normal value. I could've returned just
-- "f x", but then the old log from "(a,m)" would be lost,
-- which's not nice, so I append the old log to the new one.
-- The parameters "m" are Monoids because every monoid implements
-- the "mappend" function, used to, well, append stuff.
applyLog :: (Monoid m) => (a,m) -> (a -> (b,m)) -> (b,m)
applyLog (x,log) f = let (y,newLog) = f x in (y,log `mappend` newLog)

-- usage:
-- applyLog (3,"Smallish gang. ") isBigGang

type Food = String
type Price = Sum Int
-- I'm using a Sum of Int because Sum is an instance of the Monoid typeclass

addDrink :: Food -> (Food,Price)
addDrink "beans" = ("milk",Sum 25)
addDrink "jerky" = ("whiskey",Sum 99)
addDrink "beer" = ("a buckload of beer",Sum 300)
addDrink _ = ("beer",Sum 30)

-- now using the "addDrink" function with "applyLog"
-- applyLog ("beans",Sum 12) addDrink
-- prints: ("milk",Sum {getSum = 37})

-- applyLog ("jerky",Sum 12) addDrink
-- prints: ("whiskey",Sum {getSum = 111})

-- applyLog ("anothing else",Sum 12) addDrink
-- prints: ("beer",Sum {getSum = 42})

-- and it can also be used just like a monad, chaining operations:
-- ("food A", Sum 5) `applyLog` addDrink `applyLog` addDrink `applyLog` addDrink
-- prints: ("beer",Sum {getSum = 365})

-- simple usage of the Writer Monad, with "do" notation
logNumber :: Int -> Writer [String] Int
logNumber x = writer (x,["Got number: " ++ show x])

multWithLog :: Writer [String] Int
multWithLog = do
	a <- logNumber 3
	b <- logNumber 5
	tell ["Appending to the log with 'tell'"]
	return (a * b)

-- if you look at the Writer's implementation of the bind operation
-- it can be seen that "multWithLog" works for the same reason that
-- "example" works.
example :: Maybe Int
example = do
	a <- Just 12
	b <- Just 7
	return (a * b)

-- using Writer to log the steps taken by the gcd algorithm to compute the answer
gcd' :: Int -> Int -> Writer [String] Int
gcd' a b
	| b == 0 = do
		tell ["Finished with " ++ show a]
		return a
	| otherwise = do
		tell [show a ++ " mod " ++ show b ++ " = " ++ show (mod a b)]
		gcd' b (mod a b)
-- "gcd'" rewritten with binds instead of "do" notation
gcd'' :: Int -> Int -> Writer [String] Int
gcd'' a b
	| b == 0 = writer(a,["Finished with " ++ show a])
	| otherwise = tell newLog >> gcd' b (mod a b)
	where newLog = [show a ++ " mod " ++ show b ++ " = " ++ show (mod a b)]