remove test [] = []
remove test (x:xs) =  if test x
  then remove test xs
  else x : remove test xs

myProduct list = foldr (*) 1 list

myCycle t l =  (length filtered) /= 0 
  where filtered = filter (== t) l

fn 0 = 0
fn n =  1/n + fn (n - 1)