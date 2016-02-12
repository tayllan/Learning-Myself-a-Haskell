import System.Environment
import System.IO
import System.IO.Error

main = toTry `catchIOError` handler

toTry :: IO ()
toTry = do
	(fileName:_) <- getArgs
	contents <- readFile fileName
	putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines!"

handler :: IOError -> IO ()
handler e
	| isDoesNotExistError e =
		case ioeGetFileName e of
			Just path -> putStrLn $ "File doesn't exist at: " ++ path
			Nothing -> putStrLn "File doesn't exist at unknown location"
	| otherwise = ioError e