maximum' :: (Ord a) => [a] -> a
maximum' [] = error "No can do with empty lists"
maximum' [x] = x
maximum' (x:xs)
	| x > maxTail = x
	| otherwise = maxTail
	where maxTail = maximum' xs

maximum'' :: (Ord a) => [a] -> a
maximum'' [] = error "No can do with empty lists"
maximum'' [x] = x
maximum'' (x:xs) = max x (maximum'' xs)

replicate' :: (Num a, Ord a) => a -> t -> [t]
replicate' a t
	| a < 1 = []
	| otherwise = t:replicate' (a - 1) t

take' :: (Integral a) => a -> [t] -> [t]
take' a _
	| a < 1 = []
take' _ [] = []
take' a (x:xs) = x:take' (a - 1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

zip' :: [a] -> [b] -> [(a, b)]
zip' [] _ = []
zip' _ [] = []
zip' (a:as) (b:bs) = (a, b):zip' as bs

elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' e (x:xs)
	| e == x = True
	| otherwise = elem' e xs

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
	let
		smallerSorted = quicksort [a | a <- xs, a <= x]
		biggerSorted = quicksort [a | a <- xs, a > x]
	in smallerSorted ++ [x] ++ biggerSorted