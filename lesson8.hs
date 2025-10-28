import Text.XHtml (rev)
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

myTake 0 _ = []
myTake _ [] = []
myTake n (x:xs) = x : myTake (n-1) xs

myCycle (first:rest) = first : myCycle (rest ++ [first])

ackermann 0 n = n + 1
ackermann m 0 = ackermann (m - 1) 1
ackermann m n = ackermann (m - 1) (ackermann m (n - 1))

myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

fib _ _ 0 = 0
fib _ _ 1 = 1
fib _ _ 2 = 1
fib x y 3 = x + y
fib x y c = fib (x + y) x (c - 1)