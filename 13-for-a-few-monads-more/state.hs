import Control.Monad.State
import System.Random

type Stack = [Int]

-- the original function
--pop :: Stack -> (Int,Stack)
--pop (x:xs) = (x,xs)

-- and the version wrapped on the State monad
pop :: State Stack Int
pop = state $ \(x:xs) -> (x,xs)

-- original function
--push :: Int -> Stack -> ((),Stack)
--push x xs = ((),x:xs)

-- wrapped on State monad
push :: Int -> State Stack ()
push a = state $ \xs -> ((),a:xs)

-- original function
--stackTest :: Stack -> (Int,Stack)
--stackTest stack = let
--	((),newStack1) = push 3 stack
--	(a,newStack2) = pop newStack1
--	in pop newStack2

-- using State monad
stackTest :: State Stack Int
stackTest = do
	push 3
	pop
	pop

-- using the State monad to deal with randomness
randomState :: (RandomGen g, Random a) => State g a
randomState = state random

threeCoins :: State StdGen (Bool,Bool,Bool)
threeCoins = do
	a <- randomState
	b <- randomState
	c <- randomState
	return (a,b,c)