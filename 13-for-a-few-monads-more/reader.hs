import Control.Monad.Instances

-- example of how Function ((->) r) is an instance of Monad
-- the same parameter passed to the "addStuff" function
-- is passed to both "(*2)" and "(+10)" functions.
addStuff :: Int -> Int
addStuff = do
	a <- (*2)
	b <- (+10)
	return (a + b) -- so here "a = x * 2" and "b = x + 10"
-- this can be rewriten without the "do" notation
addStuff' :: Int -> Int
addStuff' = (*2) >>= \a -> (+10) >>= \b -> return (a + b)
-- and without monads whatsoever
addStuff'' :: Int -> Int
addStuff'' x = let
	a = (*2) x
	b = (+10) x
	in a+b