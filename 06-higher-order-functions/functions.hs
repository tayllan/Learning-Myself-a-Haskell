multiplyThree :: (Num a) => a -> a -> a -> a
multiplyThree x y z = x * y * z

compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred = compare 100

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

add :: (Num a) => a -> a -> a
add x y = x + y

addTen :: (Num a) => a -> a
addTen = add 10

zipWith' :: (Num a) => (a -> a -> a) -> [a] -> [a] -> [a]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f x y = f y x

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
	let
		smallerSorted = quicksort (filter (<=x) xs)
		biggerSorted = quicksort (filter (>x) xs)
	in smallerSorted ++ [x] ++ biggerSorted

largestDivisible :: (Integral a) => a
largestDivisible = head (filter p [100000, 99999..])
	where p x = mod x 3829 == 0

sumOddSquares :: (Integral a) => a
sumOddSquares = sum (takeWhile (< 10000) (filter odd (map (^2) [1..])))

collatzChain :: (Integral a) => a -> [a]
collatzChain 1 = [1]
collatzChain n
	| even n = n:collatzChain (n `div` 2)
	| otherwise = n:collatzChain (n * 3 + 1)

numLongChain :: Int
numLongChain = length (filter (>15) (map length (map collatzChain [1..100])))

numLongChain' :: Int
numLongChain' = length (filter (\xs -> length xs > 15) (map collatzChain [1..100]))