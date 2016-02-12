head' :: [a] -> a
head' [] = error "Empty list"
head' (x:_) = x

head'' :: [a] -> a
head'' xs = case xs of
						[] -> error "Empty list too!"
						(x:_) -> x