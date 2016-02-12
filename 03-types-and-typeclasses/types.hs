removeNonUppercase :: [Char] -> [Char]
removeNonUppercase xs = [x | x <- xs, x `elem` ['A'..'Z']]

factorial :: Integer -> Integer
factorial n = product [2..n]
