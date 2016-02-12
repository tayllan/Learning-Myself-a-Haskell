lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you're out of luck, pal!"

sayMe :: (Integral a) => a -> String
sayMe 1 = "one"
sayMe 2 = "two"
sayMe x = "Not between ONE and TWO!"

factorial' :: (Integral a) => a -> a
factorial' 0 = 1
factorial' x = x * factorial' (x - 1)

adds2D :: (Num x) => (x, x) -> (x, x) -> (x, x)
adds2D a b = (fst a + fst b, snd a + snd b)

adds2D' :: (Num x) => (x, x) -> (x, x) -> (x, x)
adds2D' (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

myFst :: (a, b, c) -> a
myFst (x, _, _) = x

mySnd :: (a, b, c) -> b
mySnd (_, y, _) = y

myTrd :: (a, b, c) -> c
myTrd (_, _, z) = z

head' :: [a] -> a
head' [] = error "Can't call head on an empty list!"
head' (x:_) = x

length' :: [a] -> Int
length' [] = 0
length' (_:xs) = 1 + length' xs

capital :: String -> String
capital "" = "Empty string, whoops!"
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

initials :: String -> String -> String
initials (f:_) (l:_) = [f] ++ " " ++ [l]
