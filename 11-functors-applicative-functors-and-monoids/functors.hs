--instance Functor (Either a) where
--	fmap :: (b -> c) -> Either a b -> Either a c
--	fmap f (Right x) = Right (f x)
--	fmap f (Left x) = Left x

data Test a = TestA a
			deriving (Show)

instance Functor Test where
	fmap function (TestA value) = TestA (function value)

main = do
	line <- fmap reverse getLine
	putStrLn $ "You said <" ++ line ++ "> backwards!"