class MyEqual a where
	isEqual :: a -> a -> Bool
	isUnequal :: a -> a -> Bool
	x `isEqual` y = not (x `isUnequal` y)
	x `isUnequal` y = not (x `isEqual` y)

data TrafficLight = Red
					| Yellow
					| Green

instance MyEqual TrafficLight where
	Red `isEqual` Red = True
	Yellow `isEqual` Yellow = True
	Green `isEqual` Green = True
	_ `isEqual` _ = False

instance Show TrafficLight where
	show Red = "RED: staaaph"
	show Yellow = "Yellow: be careful"
	show Green = "green: go on!"