
import qualified Data.Map as Map
-- 直径から面積を求める
areaGivenDiameter::Double->Double
areaGivenDiameter size = pi * (r^2)
  where r = size / 2

type Pizza = (Double, Double) -- (size, cost)

-- 1平方単位あたりの価格を求める
costPerInch::Pizza->Double
costPerInch (size, cost) = cost / area
  where area = areaGivenDiameter size

-- ピザの比較
comparePizzas::Pizza->Pizza->Pizza
comparePizzas p1 p2 = if cost1 < cost2
                      then p1
                      else p2
  where cost1 = costPerInch p1
        cost2 = costPerInch p2

-- 結果のメッセージ出力
describePizza::Pizza->String
describePizza (size, cost) = "The " ++ show size ++
                              " pizza" ++ show costSqInch ++
                              " per square inch."
  where costSqInch = costPerInch (size, cost)

constData::Map.Map Int Double
constData = Map.fromList [(1, 3.14), (2, 2.71), (3, 1.41)]

sizeData::Map.Map Int Double
sizeData = Map.fromList [(1, 12.0), (2, 10.0), (3, 8.0)]

maybeMain::Maybe String
maybeMain = do
  size1 <- Map.lookup 1 sizeData
  cost1 <- Map.lookup 1 constData
  size2 <- Map.lookup 2 sizeData
  cost2 <- Map.lookup 2 constData
  let pizza1 = (size1, cost1)
  let pizza2 = (size2, cost2)
  let betterPizza = comparePizzas pizza1 pizza2
  return (describePizza betterPizza)

main::IO ()
main = do
  putStrLn "What is size of pizza 1?"
  size1Str <- getLine
  putStrLn "What is cost of pizza 1?"
  cost1Str <- getLine
  putStrLn "What is size of pizza 2?"
  size2Str <- getLine
  putStrLn "What is cost of pizza 2?"
  cost2Str <- getLine
  let pizza1 = (read size1Str, read cost1Str)
  let pizza2 = (read size2Str, read cost2Str)
  let betterPizza = comparePizzas pizza1 pizza2
  putStrLn (describePizza betterPizza)


nameData::Map.Map Int String
nameData = Map.fromList [(1, "Margherita"), (2, "Pepperoni"), (3, "Veggie")]

helloPerson::String->String
helloPerson name = "Hello, " ++ name ++ "!"

nameFn::Maybe String
nameFn = do
  name <- Map.lookup 1 nameData
  return (helloPerson name)

fib::Int->Int->Int->Int
fib _ _ 0 = 0
fib _ _ 1 = 1
fib _ _ 2 = 1
fib x y 3 = x + y
fib x y c = fib (x + y) x (c - 1)

fibMain::IO ()
fibMain = do
  putStrLn "Enter a number:"
  num <- getLine
  let n = read num
  let result = fib 1 1 n
  putStrLn ("The " ++ show n ++ "th Fibonacci number is " ++ show result ++ ".")
