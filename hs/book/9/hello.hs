-- when
-- putStr, putStrLn
-- getLine
-- not
-- null
-- putChar, getChar
-- print
-- sequence
-- mapM, mapM_
-- forever

-- words, unwords, reverse

import Control.Monad(when, forever, forM)

-- main = do
--   putStrLn "Hello. Type your name please:"
--   name <- getLine
--   putStrLn ("Hey, " ++ name ++ ", you rock!")

--------------------------------------------------------------------------------

-- main = do
--   a <- putStrLn "First"
--   b <- putStrLn "Second"
--   return (0)

--------------------------------------------------------------------------------

-- main = do
--   line <- getLine
--   if null line
--     then return ()
--     else do
--       putStrLn $ reverseWords line
--       main
--
-- reverseWords :: String -> String
-- reverseWords = unwords . map reverse . words
-- -- reverseWords s = unwords (map reverse (words s))

--------------------------------------------------------------------------------

-- main = do
--   c <- getChar
--   when (c /= ' ') $ do
--     putChar c
--     main

--------------------------------------------------------------------------------

-- main = do
--   results <- sequence [getLine, getLine, getLine]
--   -- results <- sequence [putStrLn "faggot", putStrLn "YOU"]
--   print results

--------------------------------------------------------------------------------

-- main = forever $ do
--   putStrLn "Hey, ur name is?"
--   name <- getLine
--   putStrLn $ "Hey, " ++ name

--------------------------------------------------------------------------------

main = do
  colors <- forM [1,2,3,4] (\a -> do
    putStrLn $ "What color do you associate with number " ++ show a ++ "?"
    -- color <- getLine
    -- return color
    getLine
    )
  mapM putStrLn colors
