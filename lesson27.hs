import qualified Data.Map as Map
-- reverseMaybe::Maybe String -> Maybe String
-- reverseMaybe Nothing = Nothing
-- reverseMaybe (Just str) = Just (reverse str)

successfulRequest::Maybe Int
successfulRequest = Just 6
failedRequest::Maybe Int
failedRequest = Nothing

incMaybe::Maybe Int -> Maybe Int
incMaybe Nothing = Nothing
incMaybe (Just n) = Just (n + 1)

successStr::Maybe String
successStr = show <$> successfulRequest
failStr::Maybe String
failStr = show <$> failedRequest

maybeStr::Maybe String
maybeStr = (Just "Hello")
reverseMaybe::Maybe String->Maybe String
reverseMaybe str = reverse <$> str


-- ==============================
data RobotPart = RobotPart {
  name::String,
  description::String,
  cost::Double,
  count::Int
} deriving (Show)

leftArm::RobotPart
leftArm = RobotPart {
  name = "Left Arm",
  description = "Left arm for a robot",
  cost = 1000.00,
  count = 3
}

rightArm::RobotPart
rightArm = RobotPart {
  name = "Right Arm",
  description = "Right arm for a robot",
  cost = 1025.00,
  count = 5
}

robotHead::RobotPart
robotHead = RobotPart {
  name = "Robot Head",
  description = "Head for a robot",
  cost = 5092.25,
  count = 2
}

type Html = String

renderHtml::RobotPart->Html
renderHtml part = mconcat ["<h2>" ++ partName ++ "</h2>",
                              "<p><h3>Desc</h3>" ++ partDescription ++ "</p>",
                              "<p><h3>Cost</h3>" ++ partCost ++ "</p>",
                              "<p><h3>Count</h3>" ++ partCount ++ "</p>"]
  where partName = name part
        partDescription = description part
        partCost = show (cost part)
        partCount = show (count part)

partsDB::Map.Map Int RobotPart
partsDB = Map.fromList keyVals
  where keys = [1,2,3]
        vals = [leftArm, rightArm, robotHead]
        keyVals = zip keys vals

partVal::Maybe RobotPart
partVal = Map.lookup 1 partsDB

partHtml::Maybe Html
partHtml = renderHtml <$> partVal

allParts :: [RobotPart]
allParts = snd <$> Map.toList partsDB

allPartsHtml :: [Html]
allPartsHtml = renderHtml <$> allParts

htmlPartsDB::Map.Map Int Html
htmlPartsDB = renderHtml <$> partsDB

leftArmIo::IO RobotPart
leftArmIo = return leftArm

htmlSnippet::IO Html
htmlSnippet = renderHtml <$> leftArmIo

data Box a = Box a deriving (Show)
instance Functor Box where
  fmap f (Box x) = Box (f x)

more::Box a -> Box [a]
more (Box x) = Box [x,x,x] 