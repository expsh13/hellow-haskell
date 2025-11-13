import System.Environment (getArgs)
-- === TODO List ===
-- 1. add    - Add a new task
-- 2. list   - Show all tasks
-- 3. remove - Remove a task
-- 4. quit   - Exit

-- > add
-- Enter task: Buy milk
-- Task added!

-- > list
-- 1. Buy milk
-- 2. Write code
-- 3. Study Haskell

-- > remove
-- Enter task number: 2
-- Task removed!

-- > quit
-- Goodbye!

list:: [String] -> IO()
list [] = putStrLn "No tasks yet!"
list tasks = mapM_ printTask (zip [1..] tasks)
  where
    printTask (n, task) = putStrLn (show n ++ ". " ++ task)

addTask::[String] -> String -> [String]
addTask tasks addItem = tasks ++ [addItem]

-- 純粋関数（リスト操作）
removeAt :: Int -> [String] -> [String]
removeAt index tasks = take index tasks ++ drop (index + 1) tasks

  -- IOアクション（入力・エラー処理・ループ）
remove :: [String] -> IO ()
remove [] = do
  putStrLn "No tasks to remove!"
  mainLoop []
remove tasks = do
  putStr "Enter task number: "
  taskNum <- getLine
  let newTasks = removeAt (read taskNum - 1) tasks
  putStrLn "Task removed!"
  mainLoop newTasks


mainLoop:: [String] -> IO()
mainLoop tasks = do
  putStr "> "
  command <- getLine
  case command of
    "add" -> do
      putStr "Enter task:"
      addItem <- getLine
      let newTasks = addTask tasks addItem
      putStrLn "Task added!"
      mainLoop newTasks
    "list" -> do
      list tasks
      mainLoop tasks
    "remove" -> remove tasks
    "quit" -> putStrLn "Goodbye!"
    _ -> do
      putStrLn "Unknown command"
      mainLoop tasks

main::IO()
main = do
  putStrLn "=== TODO List ==="
  mainLoop []