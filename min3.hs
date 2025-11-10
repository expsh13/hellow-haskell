import Control.Monad.RWS (MonadState(put))
minOfThree :: (Ord a) => a -> a -> a -> a
minOfThree x y z = min x (min y z)

readInt::IO Int
readInt = read <$> getLine

minOfInts::IO Int
minOfInts = minOfThree <$> readInt <*> readInt <*> readInt

main::IO ()
main = do
  putStrLn "Enter three integers:"
  result <- minOfInts
  putStrLn ("The minimum is: " ++ show result)