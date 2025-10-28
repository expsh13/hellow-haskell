import Data.List

names =[
  ("Ian","H."),
  ("a","H."),
  ("Sato","K."),
  ("Alex","P."),
  ("Suzuki","T."),
  ("Aoi","Y."),
  ("Takahashi","R.")]

-- compareLastName name1 name2 = if lastName1 > lastName2
--                               then GT
--                               else if lastName1 < lastName2
--                                     then LT
--                                     else if firstName1 > firstName2
--                                          then GT
--                                          else if firstName1 < firstName2
--                                               then LT
--                                               else EQ
--   where lastName1 = snd name1
--         firstName1 = fst name1
--         lastName2 = snd name2
--         firstName2 = fst name2

compareLastName name1 name2 = if result == EQ
                              then compare firstName1 firstName2
                              else result
  where result = compare lastName1 lastName2
        lastName1 = snd name1
        firstName1 = fst name1
        lastName2 = snd name2
        firstName2 = fst name2


dcOffice name location = nameText ++ " - " ++ location
  where nameText = fst name ++ " " ++ snd name ++ " Esq."


