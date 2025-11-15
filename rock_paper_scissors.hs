-- === Rock Paper Scissors ===
-- Score - You: 0, Computer: 0

-- Choose: (r)ock, (p)aper, (s)cissors, (q)uit
-- > r
-- You chose: Rock
-- Computer chose: Scissors
-- You win!

-- Score - You: 1, Computer: 0

-- Choose: (r)ock, (p)aper, (s)cissors, (q)uit
-- > p
-- You chose: Paper
-- Computer chose: Paper
-- Draw!

-- Score - You: 1, Computer: 0

-- Choose: (r)ock, (p)aper, (s)cissors, (q)uit
-- > q
-- Final Score - You: 1, Computer: 0
-- Thanks for playing!

import System.Random

data Hand = Rock | Paper | Scissors deriving (Show, Eq)

computerChoice :: IO Hand
computerChoice = do
  index <- randomRIO (0, 2) :: IO Int
  return $ [Rock, Paper, Scissors] !! index

parseChoice :: String -> Maybe Hand
parseChoice "r" = Just Rock
parseChoice "p" = Just Paper
parseChoice "s" = Just Scissors
parseChoice _ = Nothing

type UserChoice = Hand

type ComChoice = Hand

type UserScore = Int

type ComScore = Int

beats :: Hand -> Hand -> Bool
beats Rock Scissors = True
beats Paper Rock = True
beats Scissors Paper = True
beats _ _ = False

judge :: UserChoice -> ComChoice -> UserScore -> ComScore -> (String, UserScore, ComScore)
judge user com us cs
  | user == com = ("Draw!", us, cs)
  | user `beats` com = ("You win!", us + 1, cs)
  | otherwise = ("You lose!", us, cs + 1)

mainLoop :: Int -> Int -> IO ()
mainLoop userScore comScore = do
  putStrLn "Choose: (r)ock, (p)aper, (s)cissors, (q)uit"
  putStr ">"
  input <- getLine

  case input of
    "q" -> do
      putStrLn ("Final Score - You:" ++ show userScore ++ ", Computer:" ++ show comScore)
      putStrLn "Thanks for playing!"
    _ -> case parseChoice input of
      Nothing -> do
        putStrLn "正しい値を入力してください"
        mainLoop userScore comScore
      Just hand -> do
        putStrLn ("You chose:" ++ show hand)
        comChoice <- computerChoice
        putStrLn ("Computer chose:" ++ show comChoice)
        let (result, newUserScore, newComScore) = judge hand comChoice userScore comScore
        putStrLn result
        putStrLn ("Score - You: " ++ show newUserScore ++ ", Computer:" ++ show newComScore)
        mainLoop newUserScore newComScore

main :: IO ()
main = do
  putStrLn "=== Rock Paper Scissors ==="
  putStrLn "Score - You: 0, Computer: 0"
  mainLoop 0 0
