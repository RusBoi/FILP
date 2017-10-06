

applyMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
applyMaybe Nothing _ = Nothing
applyMaybe (Just x) f = f x


type Birds = Int
type Pole = (Birds, Birds)

landLeft :: Birds -> Pole -> Maybe Pole
landLeft n (left, right)
  | abs (left + n - right) < 4 = Just (left + n, right)
  | otherwise  = Nothing

landRight :: Birds -> Pole -> Maybe Pole
landRight n (left, right)
  | abs (right + n - left) < 4 = Just (left, right + n)
  | otherwise = Nothing

banana :: Pole -> Maybe Pole
banana _ = Nothing

-- foo :: Maybe String
-- foo = Just 3 >>= (\x -> Just "!" >>= (\y -> Just (show x ++ y)))

foo :: Maybe String
foo = do
  x <- Just 3
  y <- Just "!"
  Just (show x ++ y)

routine :: Maybe Pole
routine = do
  start <- return (0, 0)
  first <- landLeft 2 start
  Nothing
  second <- landRight 2 first
  landLeft 1 second

justH :: Maybe Char
justH = do
  (x:xs) <- Just "hello"
  return x

--
-- listOfTuples :: [(Int,Char)]
-- listOfTuples = do
--     n <- [1,2]
--     ch <- ['a','b']
--     return (n,ch)

listOfTuples :: [(Int, Char)]
listOfTuples = [1,2] >>= \n -> ['a', 'b'] >>= \ch -> return (n, ch)

--------------------------------------------------------------------------------
-- Monad + Monoid
class Monad m => MonadPlus m where
  mzero :: m a
  mplus :: m a -> m a -> m a

instance MonadPlus [] where
  mzero = []
  mplus = (++)

guard :: (MonadPlus m) => Bool -> m ()
guard True = return ()
guard False = mzero

-- ghci> [ x | x <- [1..50], '7' `elem` show x ]
-- [7,17,27,37,47]
