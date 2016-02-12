import Control.Applicative

-- using the main monad just to test the applicative functors
main = do
	-- should print "Just 17"
	putStrLn $ show $ pure (+) <*> Just 8 <*> Just 9

	-- should print "Just (1, 3, 0)"
	putStrLn $ show $ pure (,,) <*> Just 1 <*> Just 3 <*> Just 0

	-- which is the same as
	putStrLn $ show $ fmap (,,) (Just 1) <*> Just 3 <*> Just 0
	-- because "pure f <*> x" is the same as "fmap f x"

	-- and that's why there is another infix operator for that: "<$>"
	putStrLn $ show $ (,,) <$> Just 9 <*> Just 65 <*> Just 8

	-- the above shows that, if I'm to apply a function "f" between 3 functors
	-- the syntax is: "f <$> x <*> y <*> z". And if these parameters weren't
	-- functors, just normal variables, the function could be applied as: "f x y z"!

	--ghci> pure "Hey" :: [String]
	--ghci> pure "Hey" :: Maybe String

	-- Lists as instance of Applicative Functors
	--instance Applicative [] where
	--	pure x = [x]
	--	fs <*> xs = [f x | f <- fs, x <- xs]

	-- every function from the left gets applied to every value from the right
	putStrLn $ show $ [(*0), (+100), (^2)] <*> [1, 2, 3]

	-- in the same way, every function, which receives 2 parameters, gets applied
	-- to every combination of the elementos from the left + from the right
	-- should print "[4,5,5,6,3,4,6,8]"
	-- [1 + 3, 1 + 4, 2 + 3, 2 + 4, 1 * 3, 1 * 4, 2 * 3, 2 * 4]
	putStrLn $ show $ [(+), (*)] <*> [1, 2] <*> [3, 4]

	-- if the above contained just one function as parameter, it could be used as:
	putStrLn $ show $ (+) <$> [1, 2] <*> [3, 4]
	-- that's because in the former (the first one shown), the functions are
	-- already wrapped in a functor context, that's why the normal operator "<*>"
	-- between functors works. And in the latter (the last one shown), the operator
	-- has yet to be wrapped in the functor context so that it can be used with
	-- the two functors that follow. That's why the "<$>" operator is used,
	-- to wrap "(+)" in the functor context.
	-- Remember: "f <$> x" == "fmap f x" == "pure f <*> x":
	putStrLn $ show $ (+) <$> [1, 2] <*> [3, 4]
	putStrLn $ show $ fmap (+) [1, 2] <*> [3, 4]
	putStrLn $ show $ pure (+) <*> [1, 2] <*> [3, 4]

	-- Applicative Functors can be used to replace list comprehensions:
	-- instead of doing
	putStrLn $ show $ [x*y | x <- [2,5,10], y <- [8,10,11]]
	-- I could do:
	putStrLn $ show $ (*) <$> [2,5,10] <*> [8,10,11]

	-- IO String with applicative style (commented so it doesn't stop the process in the middle)
	-- (++) <$> getLine <*> getLine
	-- instead of doing something like this:
	-- a <- getLine
	-- b <- getLine
	-- a ++ b

	-- Functions as Applicative Functors (weird)
	putStrLn $ show $ (+) <$> (+3) <*> (*100) $ 5
	-- 5 gets multiplied by 100, 5 gets summed to 3, and then (5*100) gets summed (+) to (5+3)

	-- it should print "[8.0,10.0,2.5]"
	putStrLn $ show $ (\x y z -> [x,y,z]) <$> (+3) <*> (*2) <*> (/2) $ 5

	-- ZipLists: used so that something like "[(+3),(*2)] <*> [1,2]" results in
	-- "[3 + 1, 2 * 2]", instead of the real result "[4, 5, 2, 4]"
	-- it should print "[4, 4]"
	putStrLn $ show $ getZipList $ ZipList [(+3), (*2)] <*> ZipList [1, 2]

	-- it should print "[4, 10]"
	putStrLn $ show $ getZipList $ (+) <$> ZipList [3, 2] <*> ZipList [1, 8]