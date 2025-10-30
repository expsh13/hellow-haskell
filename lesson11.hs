printDouble::Int -> String
printDouble n =show (n * 2)

filter::(a->Bool) -> [a] -> [a]
filter test [] = []


tail::[a] -> [a]
tail (_:xs) = xs

head::[a] -> a
head (x:_) = x

myFoldl::(a->b->a) -> a -> [b] -> a
myFoldl f init [] = init
myFoldl f init (x:xs) = myFoldl f newInit xs
  where newInit = f init x