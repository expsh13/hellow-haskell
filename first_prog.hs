toPart recipient = "Dear " ++ recipient ++ ",\n"
bodyPart bookTitle = "Thanks for buying " ++ bookTitle ++ ".\n"
fromPart author = "Thanks,\n" ++ author

createEmail recipient bookTitle author =
    toPart recipient ++
    bodyPart bookTitle ++
    fromPart author

simple x = x


double x = double*2 where double = x*2

overwrite x = (\x -> (\x -> (\x -> x)4) 3)2

main = do
  print "Who is the email for????"
  recipient <- getLine
  print "What is the book title?"
  bookTitle <- getLine
  print "Who is the author?"
  author <- getLine
  print (createEmail recipient bookTitle author)

