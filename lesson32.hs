import Data.Char (toUpper)

fn::[(Int,Int)]
fn = do
  num <- [1..10]
  return (num, num^2)

powerOfTwoAndThree:: Int -> [(Int, Int)]
powerOfTwoAndThree n = [(powerOfTwo, powerOfThree) 
                        | value <- [1..n]
                        , let powerOfTwo = 2 ^ value
                        , let powerOfThree = 3 ^ value]

toUppered::[String] -> [String]
toUppered strs = ["Mr. " ++ upperedStr 
                | str <- strs
                , let firstStr = toUpper (head str)
                , let tailStr = tail str
                , let upperedStr = firstStr : tailStr]