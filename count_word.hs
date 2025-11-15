-- \$ cat input.txt
-- the quick brown fox jumps over the lazy dog
-- the fox is quick

-- \$ runghc wordcount.hs input.txt
-- Total words: 13
-- Unique words: 10

-- Top 5 words:
-- the: 3
-- fox: 2
-- quick: 2
-- brown: 1
-- jumps: 1

import Data.List (sortOn)
import qualified Data.Map as Map
import Data.Ord (Down (..))

countWords :: [String] -> Map.Map String Int
countWords = foldr increment Map.empty
  where
    increment word = Map.insertWith (+) word 1

topWords :: Map.Map String Int -> [(String, Int)]
topWords = take 5 . sortOn (Down . snd) . Map.toList

main :: IO ()
main = do
  content <- readFile "input.txt"
  let wordList = words content
      counts = countWords wordList
  putStrLn $ "Total words: " ++ show (length wordList)
  putStrLn $ "Unique words: " ++ show (Map.size counts)
  putStrLn "Top 5 words:"
  mapM_ (\(word, num) -> putStrLn (word ++ ": " ++ show num)) (topWords counts)
