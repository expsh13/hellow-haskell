import Text.XHtml (ins)
myAny :: (a -> Bool) -> [a] -> Bool
myAny test = (foldr (||) False) . (map test)


data Color = Red | Green | Blue | Yellow | Purple | Orange | Brown | Clear deriving (Show,Eq)
instance Semigroup Color where
  (<>) Red Blue = Purple
  (<>) Blue Red = Purple
  (<>) Red Yellow = Orange
  (<>) Yellow Red = Orange
  (<>) Blue Yellow = Green
  (<>) Yellow Blue = Green
  (<>) c1 c2
    | c1 == c2 = c1
    | otherwise = Brown
instance Monoid Color where
  mempty = Clear
  mappend col1 col2 = col1 <> col2


type Events = [String]
type Probs = [Double]
data PTable = PTable Events Probs

instance Semigroup PTable where
  (<>) :: PTable -> PTable -> PTable
  (<>) ptable1 (PTable [] []) = ptable1
  (<>) (PTable [] []) ptable2 = ptable2
  (<>) (PTable e1 p1) (PTable e2 p2) = createPTable newEvents newProbs
    where newEvents = combineEvents e1 e2
          newProbs = combineProbs p1 p2

createPTable :: Events -> Probs -> PTable
createPTable events probs = PTable events normalizedProbs
  where totalProbs = sum probs
        normalizedProbs = map (\p -> p / totalProbs) probs

instance Monoid PTable where
  mempty = PTable [] []
  mappend = (<>)

showPair::String->Double->String
showPair event prob = mconcat [event, ":", show prob, "\n"]
instance Show PTable where
  show (PTable events probs) = mconcat pairs
    where pairs = zipWith showPair events probs

cartCombine::(a->b->c) -> [a] -> [b] -> [c]
cartCombine f l1 l2 = zipWith f newL1 cycledL2
  where nToAdd = length l2
        repeatedL1 = map (take nToAdd . repeat) l1
        newL1 = mconcat repeatedL1
        cycledL2 = cycle l2

combineEvents::Events->Events->Events
combineEvents e1 e2 = cartCombine combiner e1 e2
  where combiner = \x y -> mconcat [x, "-", y]

combineProbs::Probs->Probs->Probs
combineProbs p1 p2 = cartCombine (*) p1 p2