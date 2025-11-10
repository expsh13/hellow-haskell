-- 基本問題:
-- 1から100までの数字を出力してください。ただし：

-- 3の倍数のときは数の代わりに「Fizz」と出力
-- 5の倍数のときは数の代わりに「Buzz」と出力
-- 3と5の両方の倍数のときは「FizzBuzz」と出力

-- 1から100までの数字
-- nums::[Int]
-- nums = [1..100]

-- fizz::Int -> Bool
-- fizz num = num `mod` 3 == 0

-- buzz::Int -> Bool
-- buzz num = num `mod` 5 == 0


-- fizzBuzz :: Int -> Bool
-- fizzBuzz num = num `mod` 15 == 0

-- toFizzBuzz :: Int -> String
-- toFizzBuzz num
--   | fizzBuzz num = "FizzBuzz"
--   | buzz num     = "Buzz"
--   | fizz num     = "Fizz"
--   | otherwise    = show num

-- calc::[String]
-- calc = toFizzBuzz <$> nums

-- main::IO()
-- main = do
--   putStrLn "1から100までの数字のFizzBuzzを出力します。"
--   mapM_ putStrLn calc

toFizzBuzz::Int->String
toFizzBuzz n =case (n `mod` 3 == 0,n `mod` 5 == 0) of
              (True, True)  -> "FizzBuzz"
              (True, False) -> "Fizz"
              (False, True) -> "Buzz"
              _             -> show n

main::IO()
main = mapM_ (putStrLn . toFizzBuzz)[1..100]