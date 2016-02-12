type FirstName = String

data Person = Person {
	firstName :: FirstName,
	lastName :: String,
	age :: Int,
	height :: Float,
	phoneNumber :: String,
	flavor :: String
} deriving (Show)