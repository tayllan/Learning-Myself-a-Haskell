bodyMassIndex :: (RealFloat a) => a -> a -> String
bodyMassIndex weight height
	| bmi <= skinny = "Underweight"
	| bmi <= normal = "Normal"
	| bmi <= fat = "Fat"
	| otherwise = "Obese"
	where
		bmi = weight / height ^ 2
		skinny = 18.5
		normal = 25.0
		fat = 30.0

calcBodyMassIndexes :: (RealFloat a) => [(a, a)] -> [a]
calcBodyMassIndexes xs = [bmi w h | (w, h) <- xs]
	where bmi weight height = weight / height ^ 2

calcBodyMassIndexes' :: (RealFloat a) => [(a, a)] -> [a]
calcBodyMassIndexes' xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]

max' :: (Ord a) => a -> a -> a
max' x y
	| x > y = x
	| otherwise = y

myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
	| a > b = GT
	| a == b = EQ
	| otherwise = LT

initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ ". "
	where
		(f:_) = firstname
		(l:_) = lastname

cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
	let
		sideArea = 2 * pi * r * h
		topArea = pi * r ^ 2
	in sideArea + 2 * topArea