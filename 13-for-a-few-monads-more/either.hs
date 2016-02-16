-- rewriting the example of the "tightrope walker" found on
-- "12-a-fistful-of-monads/monads.hs" using "Either" instead of "Maybe"
import Control.Monad.Error

type Birds = Int
type Pole = (Birds, Birds)

landLeft :: Birds -> Pole -> Either String Pole
landLeft n (left,right)
	| abs ((left + n) - right) < 4 = Right (left + n, right)
	| otherwise = Left $ "Too many birds on the left side: (" ++ (show $ left + n) ++ "," ++ (show right) ++ ")"

landRight :: Birds -> Pole -> Either String Pole
landRight n (left,right)
	| abs (left - (right + n)) < 4 = Right (left, right + n)
	| otherwise = Left $ "Too many birds on the right side: (" ++ (show left) ++ "," ++ (show $ right + n) ++ ")"

routine :: Either String Pole
routine = return (0, 0) >>= landLeft 2 >>= landRight 2 >>= landLeft 1