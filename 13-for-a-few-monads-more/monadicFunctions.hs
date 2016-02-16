import Control.Monad
import Control.Applicative
import Control.Monad.State
import Control.Monad.Writer

-- auxiliary functions and types
type Stack = [Int]
pop :: State Stack Int
pop = state $ \(x:xs) -> (x,xs)

keepSmall :: Int -> Writer [String] Bool
keepSmall x
	| x < 4 = do
		tell ["Keeping " ++ show x]
		return True
	| otherwise = do
		tell [show x ++ " is too large, throwing it away"]
		return False

smallerThanNine :: Int -> Int -> Maybe Int
smallerThanNine acc x
	| x > 8 = Nothing
	| otherwise = Just (acc + x)

-- using the main monad to print stuff
main = do
	-- "liftM" -> it takes a function and a Monad, and maps the function
	-- over the Monad. Much like what "fmap" does to Functors.
	putStrLn $ show $ liftM (*2) [1..10]
	putStrLn $ show $ fmap (*2) [1..10] -- comparing with fmap

	putStrLn $ show $ liftM (>7) (Just 8)
	putStrLn $ show $ fmap (>7) (Just 8)

	putStrLn $ show $ liftM (>7) Nothing
	putStrLn $ show $ fmap (>7) Nothing

	putStrLn $ show $ liftM (>7) (Left "error")

	-- for "Writer"s, the liftM function gets mapped to the first value
	-- of the tuple, which is the result (the second one is just the log).
	putStrLn $ show $ runWriter $ liftM not $ writer (True,"striiing")

	-- "liftM" over a stateful computation returns another stateful computation,
	-- one in which the result is mapped with the given function.
	putStrLn $ show $ runState (liftM (+100) pop) [1,2,3,4]
	-- prints: "(101,[2,3,4])"
	putStrLn $ show $ runState pop [1,2,3,4]
	-- and whitout the mapping given by "liftM", it prints: "(1,[2,3,4])"

	-- "ap" takes a function that's inside a context, and a monad
	-- (a value inside a context), takes both of them out of their
	-- contexts, apply the function to the value, and returns them
	-- inside a context again. Its implementation:
	--ap :: (Monad m) => m (a -> b) -> m a -> m b
	--	ap mf m = do
	--	f <- mf
	--	x <- m
	--	return (f x)
	-- i.e. it's much like the "<*>" function for Applicative Functors

	putStrLn $ show $ Just (+2) <*> Just 4 -- prints: "Just 6"
	putStrLn $ show $ Just (+2) `ap` Just 4 -- the same as above

	putStrLn $ show $ [(*10),(*20)] <*> [1,2,3] -- prints: "[10,20,30,20,40,60]"
	putStrLn $ show $ [(*10),(*20)] `ap` [1,2,3] -- same

	-- the "join" function works as a "flattening function", in which
	-- it takes a monad that has a monad inside it, and just returns
	-- a normal monad
	putStrLn $ show $ join $ Just $ Just 12 -- prints: "Just 12"
	putStrLn $ show $ (join $ Just Nothing :: Maybe Int) -- prints: "Nothing"

	-- the below doesn't work because the "join" function demands
	-- that the type of both monads (the outer and the inner one)
	-- be the same
	-- putStrLn $ show $ join $ Just $ [1..10]

	-- flattening a list:
	putStrLn $ show $ join $ [[1,2],[3],[4,5]] -- prints: "[1,2,3,4,5]"

	-- "m >>= f" is always the same as "join $ fmap f x":
	putStrLn $ show $ join $ fmap (\x -> Just (x + 12)) (Just 90) -- prints: "Just 102"
	putStrLn $ show $ (Just 90) >>= (\x -> Just (x + 12)) -- same

	putStrLn $ show $ fst $ runWriter $ filterM keepSmall [9,1,5,2,10,3] -- prints: "[1,2,3]"
	-- and this prints the logs: the 9 was thrown away, the 1 kept, etc.
	mapM_ putStrLn $ snd $ runWriter $ filterM keepSmall [9,1,5,2,10,3]

	-- the monadic equivalent of "foldl" -> "foldM"
	putStrLn $ show $ foldM smallerThanNine 0 [2,8,(-1)] -- prints: "Just 9"
	putStrLn $ show $ foldM smallerThanNine 0 [2,11,90,8,(-1)] -- prints: "Nothing"
	-- the above failed (printed "Nothing"),
	-- because one number wasn't smaller than 9

	-- monadic function composition is done using the "<=<" operator
	-- normal function composition:
	putStrLn $ show $ ( (+1) . (*100) ) 4 -- prints: "401" -> (4 * 100) + 1

	-- monadic function composition
	putStrLn $ show $ (Just 4) >>= ( (\x -> return (x + 1)) <=< (\x -> return (x * 100)) )
	-- prints: "Just 401"