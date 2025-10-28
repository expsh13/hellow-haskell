import Text.XHtml (rev)
repeat x = tail (cycle [x])

subseq s f l = drop s (reverse(drop f (reverse l)))

inFirstHalf x l = elem x (take halfLen l)
  where halfLen = if length l `mod` 2 == 0
                   then length l `div` 2
                   else (length l `div` 2) - 1


sample 2 = 2*10

myTail (_:xs) = xs
myTail [] = []

myGCD a 0 = a
myGCD a b = myGCD b (a `mod` b)