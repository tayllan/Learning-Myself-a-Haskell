-- main = putStrLn "hello, world!"

--main = do
--	putStrLn "Type a letter: "
--	letter <- getLine
--	putStrLn ("This is what you letter gets you: " ++ tellWhat letter)
--	where tellWhat (x:_) = x : " lame!"

import Data.Char

--main = do
--	putStrLn "What's your first name?"
--	firstName <- getLine
--	putStrLn "What's your last name?"
--	lastName <- getLine
--	let
--		bigFirstName = map toUpper firstName
--		bigLastName = map toUpper lastName
--	putStrLn $ "hey " ++ bigFirstName ++ " " ++ bigLastName ++ ", how are you?"

main = do
	line <- getLine
	if null line
		then do
			putStrLn "ending this madness"
			return ()
		else do
			putStrLn $ reverseWords line
			main

reverseWords :: String -> String
reverseWords = unwords . map reverse . words