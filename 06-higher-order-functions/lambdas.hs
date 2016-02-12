collatzChain :: (Integral a) => a -> [a]
collatzChain 1 = [1]
collatzChain n
	| even n = n:collatzChain (n `div` 2)
	| otherwise = n:collatzChain (n * 3 + 1)

numLongChain :: Int
numLongChain = length (filter (\xs -> length xs > 15) (map collatzChain [1..100]))

sum' :: (Num a) => [a] -> a
sum' xs = foldl (\accumulator x -> accumulator + x) 0 xs

sum'' :: (Num a) => [a] -> a
sum'' = foldl (+) 0

elem' :: Eq t => t -> [t] -> Bool
elem' e = foldl (\acc x -> if x == e then True else acc) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> f x : acc) []

maximum' :: (Ord a) => [a] -> a
maximum' xs = foldl1 (\acc x -> max acc x) xs

reverse' :: [a] -> [a]
reverse' = foldl (\acc x -> x : acc) []

product' :: (Num a) => [a] -> a
product' xs = foldr1 (*) xs

filter' :: (a -> Bool) => [a] -> [a]
filter' f = foldl (\acc x -> if f x then x : acc else acc) []

head' :: [a] -> a
head' = foldr1 (\x _ -> x)

last' :: [a] -> a
last' = foldl1 (\_ x -> x)

sqrtSums :: Int
sqrtSums = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) + 1

-- function composition
oddSquareSum :: Integer
oddSquareSum = sum . takeWhile (<10000) . filter odd . map (^2) $ [1..]