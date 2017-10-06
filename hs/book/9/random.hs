-- random
-- randoms
-- randomR
-- randomRs
-- mkStdGen
-- splitAt

import System.Random
import Control.Monad(when)
import Data.List


threeCoins :: StdGen -> (Bool, Bool, Bool)
threeCoins gen =
  let (firstCoin, newGen) = random gen
      (secondCoin, newGen1) = random newGen
      (thirdCoin, newGen2) = random newGen1
  in (firstCoin, secondCoin, thirdCoin)

randoms' :: (RandomGen g, Random a) => g -> [a]
randoms' gen =
  let (newVal, newGen) = random gen
  in newVal:randoms' newGen

finiteRandoms :: (RandomGen g, Random a) => Int -> g -> ([a], g)
finiteRandoms 0 gen = ([], gen)
finiteRandoms n gen =
  let  (value, newGen) = random gen
       (restOfList, finalGen) = finiteRandoms (n-1) newGen
  in (value : restOfList, finalGen)

randomRs' :: (RandomGen g, Random a) => (a, a) -> g -> [a]
randomRs' b gen =
  let (newVal, newGen) = randomR b gen
  in newVal:randomRs' b newGen


-------------------------------------------------------------------------------

-- main = do
  -- gen <- getStdGen
  -- let randomChars = randomRs ('a', 'z') gen
  --     (first20, rest) = splitAt 20 randomChars
  --     (second20, _) = splitAt 20 rest
  --
  -- putStrLn first20
  -- putStrLn second20
  -----------------------------OR-----------------------------
  -- gen <- getStdGen
  -- putStrLn $ take 20 (randomRs ('a', 'z') gen)
  -- gen' <- newStdGen -- getStdGen второй раз вернет тот же генератор
  -- putStrLn $ take 20 (randomRs ('a', 'z') gen')

main = do
  gen <- getStdGen
  askForNumber gen

askForNumber :: StdGen -> IO ()
askForNumber gen = do
  let (randNumber, newGen) = randomR (0, 10) gen :: (Int, StdGen)
  putStrLn "Which number between 1 and 10 you choose?"
  input <- getLine
  when (not $ null input) $ do
    let number = read input :: Int
    if randNumber == number
      then putStrLn "You gessed it!!!"
      else putStrLn $ "No,  it was " ++ show randNumber
    askForNumber newGen
