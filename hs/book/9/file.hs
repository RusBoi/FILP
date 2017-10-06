-- getContents
-- interact

-- openFile
-- IOMode, FilePath
-- hGetContents
-- hClose
-- withFile

-- hGetLine
-- hPutStr
-- hPutStrLn

-- readFile
-- writeFile
-- appendFile

-- hSetBuffering
-- hFlush


import Control.Monad
import Data.Char
import System.IO
import System.Directory
import Data.List


-- main = forever $ do
--   putStr "Give me some input: "
--   l <- getLine
--   putStrLn $ map toUpper l

--------------------------------------------------------------------------------

-- main = do
--   contents <- getContents
--   putStr $ shortLinesOnly contents
--
-- shortLinesOnly :: String -> String
-- shortLinesOnly s = unlines $ filter (\l -> length l < 10) $ lines s


--------------------------------------------------------------------------------

-- main = interact $ unlines . filter ((<10) . length) . lines

--------------------------------------------------------------------------------

-- main = interact filterPalindroms
--
-- filterPalindroms :: String -> String
-- filterPalindroms = unlines . filter isPalindrom . lines
--   where isPalindrom l = l == reverse l

--------------------------------------------------------------------------------

-- main = do
--   handle <- openFile "sample.txt" ReadMode
--   contents <- hGetContents handle
--   putStr contents
--   hClose handle

--------------------------------------------------------------------------------

-- main = do
--   withFile "sample.txt" ReadMode (\handle -> do
--     t <- hGetContents handle
--     putStr t)
--
--
-- withFile' :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
-- withFile' path mode f = do
--   handle <- openFile path mode
--   res <- f handle
--   hClose handle
--   return res

--------------------------------------------------------------------------------

-- main = do
--   contents <- readFile "sample.txt"
--   putStr contents


-- main = do
--   withFile "sample.txt" ReadMode (\handle -> do
--     hSetBuffering handle $ BlockBuffering (Just 2048)
--     contents <- hGetContents handle
--     putStr contents)


main = do
  handle <- openFile "todo.txt" ReadMode
  (tempName, tempHandle) <- openTempFile "." "temp" -- !
  contents <- hGetContents handle
  let todoTasks = lines contents
      numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks
  putStrLn "These are your TO-DO items:"
  putStr $ unlines numberedTasks
  putStrLn "Which one do you want to delete?"
  numberString <- getLine
  let number = read numberString
      newTodoItems = delete (todoTasks !! number) todoTasks
  hPutStr tempHandle $ unlines newTodoItems
  hClose handle
  hClose tempHandle
  removeFile "todo.txt"           -- !
  renameFile tempName "todo.txt"  -- !
