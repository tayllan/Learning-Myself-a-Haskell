import System.IO
import Data.Char

--main = do
--	handle <- openFile "something.txt" ReadMode
--	contents <- hGetContents handle
--	putStr contents
--	hClose handle

--main = do
--	withFile "something.txt" ReadMode (
--		\handle -> do
--			contents <- hGetContents handle
--			putStr contents
--		)

--main = do
--	handle <- openFile "lero.txt" WriteMode
--	hPutStrLn handle "something and then lero and then nothing"
--	hClose handle

--main = do
--	contents <- readFile "something.txt"
--	putStr contents

main = do
	contents <- readFile "something.txt"
	writeFile "somethingcaps.txt" (map toUpper contents)