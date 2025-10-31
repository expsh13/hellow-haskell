type PatientName = (String, String)
type Age = Int
type Height = Int
data Name = Name String String | NameWithMiddle String String String 
data Sex = Male | Female
data RhType = Pos | Neg
data ABOType = A | B | AB | O
data BloodType = BloodType ABOType RhType

showName::Name -> String
showName (Name first last) = first ++ " " ++ last
showName (NameWithMiddle first middle last) = first ++ " " ++ middle ++ " " ++ last

-- data Patient = Patient Name Sex Age Height Int BloodType
data Patient = Patient {
  name :: Name,
  sex :: Sex,
  age :: Age,
  height :: Height,
  weight :: Int,
  bloodType :: BloodType
}

janeElizabethSmith::Patient
janeElizabethSmith = Patient (NameWithMiddle "Jane" "Elizabeth" "Smith") Female 21 62 115 (BloodType O Pos)

jackieSmith::Patient
jackieSmith = Patient {
  name = Name "Jackie" "Smith",
  sex = Female,
  age = 43,
  height = 65,
  weight = 130,
  bloodType = BloodType A Neg
}

patientSummary::Patient -> String
patientSummary patient = "**********\n" ++
                         "Patient Name: " ++ showName (name patient) ++ "\n" ++
                        --  "Sex: " ++ show (sex patient) ++ "\n" ++
                         "Age: " ++ show (age patient) ++ "\n" ++
                         "Height: " ++ show (height patient) ++ "\n" ++
                         "Weight: " ++ show (weight patient) ++ "\n" ++
                        --  "Blood Type: " ++ show (bloodType patient) ++ "\n" ++
                         "**********"