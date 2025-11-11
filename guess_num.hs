-- 要件:

-- ユーザーに数字を入力させる
-- 入力された数字が正解より大きければ「Lower!」と表示
-- 入力された数字が正解より小さければ「Higher!」と表示
-- 正解したら「Correct! You guessed it in X tries!」と表示して終了
-- 試行回数をカウントする

checkNum :: Int -> Int -> IO ()
checkNum targetNum count = do
  num <- readLn
  case compare num targetNum of
    -- シーケンス演算子で左の結果を捨てる
    GT -> do
      putStrLn "Higher!"
      checkNum targetNum (count+1)
    LT -> do
      putStrLn "Lower!"
      checkNum targetNum (count+1)
    EQ -> putStrLn ("Correct! You guessed it in " ++ show (count+1) ++ " tries!!")

main::IO()
main = do
  let targetNum = 50
  putStrLn "予想する数値を入力してください。"
  checkNum targetNum 0
