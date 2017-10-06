import Data.Char (toUpper)
import Control.Applicative

-- instance Functor IO where
--   fmap f action = do
--     result <- action
--     return (f result)

-- main = do
--   putStrLn =<< fmap (reverse . map toUpper) getLine

-- instance Functor ((->) r) where
--   fmap = (.)

-- The first functor law states that if we map the id function over a functor,
-- the functor that we get back should be the same as the original functor

-- The second law says that composing two functions and then mapping the
-- resulting function over a functor should be the same as first mapping
-- one function over the functor and then mapping the other one.

-- Приведем пример, когда коснтруктор типа является экзмепляра type-class'а Functor,
-- но не саблюдает два закона

data CMaybe a = CNothing | CJust Int a deriving (Show)

instance Functor CMaybe where
  fmap f CNothing = CNothing
  fmap f (CJust counter value) = CJust (counter+1) (f value)

--------------------------------------------------------------------------------
-- Если мы в fmap первым аргументом засунем функцию, которая к примеру
-- берет не один аргумент (как якобы должно), а два аргумента, то на выходе
-- fmap вокруг функтора мы получим ФУНКТОР, который содержит в себе функции

-- пример: :t fmap (*) [1, 2, 3] :: (Num a) => [a -> a]
-- то есть на выходе мы получим список функций. ТО есть в функторе уже будут функции
-- Было бы круто если бы мы могли как нибудь извель эти функции из функтора...
-- Для этого служит Applicative Функтор

{-
class (Functor f) => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
-}

-- pure помещает значение в коробку (функтор)
-- <*> - делает тоже самое что и fmap, только первый аргумент это не просто функция
-- а функция завернутая в функтор. То есть <*> как-то извлекает функцию из коробки
-- (<*>) (Just (+10)) (Just 1) = Just 11

{-
instance Applicative Maybe where
  pure = Just
  Nothing <*> _ = Nothing
  (Just f) <*> something = fmap f something
-}

-- Благодаря всему этому добру можно делать превосходные штуки:
-- pure f <*> x <*> y <*> ... <*>
-- fmap f x <*> y <*> ... <*>
-- f <$> x <*> y <*> ... <*> // тут <$> == fmap (инфиксный аналог)
-- (++) <$> Just "hahaha" <*> Just " u r loser" == Just "hahaha u r loser"

{-
instance Applicative [] where
  pure x = [x]
  fs <*> xs = [f x | f <- fs, x <- xs]
-}

-- pure 10 :: [Int]
-- pure 10 :: Maybe Int - выведут разный результат

{-
instance Applicative IO where
  pure x = return (x)
  l <*> r = do
    func <- l
    x <- b
    return (func x)
-}

main = do
  a <- (++) <$> getLine <*> getLine
  putStrLn a

{-
instance Applicative ((->) r) where
  pure x = (\_ -> x)
  f <*> g = \x -> f x (g x)
-}

-- (+) <$> (+3) <*> (*100) $ 5 = 508 )))
-- (\x y z -> [x,y,z]) <$> (+3) <*> (*2) <*> (/2) $ 5


{-
instance Applicative ZipList where
  pure x = ZipList $ repeat x
  ZipList fs <*> ZipList xs = ZipList (zipWith (\(f, x) -> f x) fs xs)
-}

-- (,,) = \(x, y, z) -> (x, y, z)
-- getZipList $ (,,) <$> ZipList "hey" <*> ZipList "lol" <*> ZipList "kek"
-- это аналог zip3
-- getZipList $ (+) <$> ZipList [1,2,3] <*> ZipList [100,100,100]

liftA2' :: (Applicative f) => (a -> b -> c) -> f a -> f b -> f c
liftA2' f a b = f <$> a <*> b

-- (:) <$> Just 3 <*> Just [4] == liftA2 (:) (Just 2) (Just [3])


sequenceA' :: (Applicative f) => [f a] -> f [a]
sequenceA' [] = pure []
sequenceA' (x:xs) = (:) <$> x <*> sequenceA' xs
-- sequenceA' = foldr (liftA2 (:)) (pure [])

-- sequenceA [(Just 1), (Just2)] = Just [1,2]


----
