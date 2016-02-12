import qualified Tests.List as L
import qualified Tests.Maybe as M
import qualified Tests.Tree as T

main = do
	putStrLn $ "Are Lists functors? <" ++ (show L.isFunctor) ++ ">"
	putStrLn $ "Are Lists applicative functors? <" ++ (show L.isApplicative) ++ ">"
	putStrLn $ "Are Lists monads? <" ++ (show L.isMonad) ++ ">"
	putStrLn ""

	putStrLn $ "Is Maybe a functor? <" ++ (show M.isFunctor) ++ ">"
	putStrLn $ "Is Maybe an applicative functor? <" ++ (show M.isApplicative) ++ ">"
	putStrLn $ "Is Maybe a monad? <" ++ (show M.isMonad) ++ ">"
	putStrLn ""

	putStrLn $ "Are Trees functors? <" ++ (show T.isFunctor) ++ ">"
	putStrLn $ "Are Trees applicative functors? <" ++ (show T.isApplicative) ++ ">"
	putStrLn $ "Are Trees monads? <" ++ (show T.isMonad) ++ ">"
	putStrLn ""