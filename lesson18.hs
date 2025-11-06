import qualified Data.Map as Map
import Data.List (intercalate)
import Data.Maybe (isNothing)

data Box a = Box a deriving (Show)

wrap::a->Box a
wrap x = Box x

unwrap::Box a->a
unwrap (Box x) = x

data Triple a = Triple a a a deriving (Show)



type Point3D = Triple Double
aPoint::Point3D
aPoint = Triple 1.0 2.0 3.0

first::Triple a->a
first (Triple x _ _) = x
second::Triple a->a
second (Triple _ y _) = y
third::Triple a->a
third (Triple _ _ z) = z

toList::Triple a->[a]
toList (Triple x y z) = [x,y,z]

transform::(a->a)->Triple a->Triple a
transform f (Triple x y z) = Triple (f x) (f y) (f z)

boxMap::(a->b)->Box a -> Box b
boxMap f (Box x) = Box (f x)

tripleMap::(a->b)->Triple a->Triple b
tripleMap f (Triple x y z) = Triple (f x) (f y) (f z)

data List a = Empty | Cons a (List a) deriving (Show)

data Organ = Heart | Brain | Kidney | Spleen deriving (Show, Eq)

organs::[Organ]
organs = [Heart,Heart,Brain,Spleen,Spleen,Kidney]

ids::[Int]
ids = [2,7,13,14,21,24]

organPairs::[(Int,Organ)]
organPairs = zip ids organs

organCatalog::Map.Map Int Organ
organCatalog = Map.fromList organPairs

possibleDrawers::[Int]
possibleDrawers = [1..50]

getDrawerContents::[Int]->Map.Map Int Organ->[Maybe Organ]
getDrawerContents ids catalog = map getContents ids
  where getContents = \id -> Map.lookup id catalog

availableOrgans::[Maybe Organ]
availableOrgans = getDrawerContents possibleDrawers organCatalog

countOrgan::Organ->[Maybe Organ]->Int
countOrgan organ available = length (filter (\x -> x == Just organ) available)

isSomething::Maybe Organ->Bool
isSomething Nothing = False
isSomething (Just _) = True

justTheOrgans::[Maybe Organ]
justTheOrgans = filter isSomething availableOrgans

showOrgan::Maybe Organ -> String
showOrgan (Just organ) = show organ
showOrgan Nothing = ""

organList::[String]
organList = map showOrgan justTheOrgans

cleanList::String
cleanList = intercalate ", " organList

-- numOrZero::Maybe Int -> Int
-- numOrZero Nothing = 0
-- numOrZero (Just x) = x

data Container = Vat Organ | Cooler Organ | Bag Organ

instance Show Container where
  show (Vat organ) = show organ ++ " in a vat"
  show (Cooler organ) = show organ ++ " in a cooler"
  show (Bag organ) = show organ ++ " in a bag"

data Location = Lab | Kitchen | Bathroom deriving (Show)

organToContainer::Organ->Container
organToContainer Brain = Vat Brain
organToContainer Heart = Cooler Heart
organToContainer organ = Bag organ

placeInLocation::Container->(Location,Container)
placeInLocation (Vat a) = (Lab, Vat a)
placeInLocation (Cooler a) = (Kitchen, Cooler a)
placeInLocation (Bag a) = (Bathroom, Bag a)

process::Organ->(Location,Container)
process organ = placeInLocation (organToContainer organ)

report::(Location,Container)->String
report (location,container) = show container ++ " in the " ++ show location

processAndRequest::(Maybe Organ)->String
processAndRequest Nothing = "Organ not found"
processAndRequest (Just organ) = report (process organ)

processRequest::Int->Map.Map Int Organ->String
processRequest id catalog = processAndRequest organ
  where organ = Map.lookup id catalog


emptyDrawers :: [Maybe Organ] -> Int
emptyDrawers list = (length . filter isNothing) list

fn::(a->b)-> Maybe a -> Maybe b
fn f Nothing = Nothing
fn f (Just x) = Just (f x)