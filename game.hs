data User = User {
  name::String,
  gamerId::Int,
  score::Int
} deriving (Show)

serverGamerId = Just 3
serverScore = Just 9001