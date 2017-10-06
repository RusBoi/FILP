-- all



import Data.Monoid
import qualified Data.Foldable as F

-- моноид - это когда у тебя есть ассоциативная бинарная функция и нейтральный элемент относительно ее
-- TYpeclass Monoid придуман как раз, чтобы подчеркнуть что тип относится к моноидам

-- class Monoid m where
--   mempty :: m
--   mappend :: m -> m -> m
--   mconcat :: [m] -> m
--   mconcat = foldr mappend mempty
-- заметим, что инстансом моноида может быть только конкретный тип, а не типовый конструктор как в примере с Functor


-- mempty `mappend` x = x
-- x `mappend` mempty = x
-- (x `mappend` y) `mappend` z = x `mappend` (y `mappend` z)
-- (правила моноидов в функция typeclass'а)

-- instance Monoid [a] where
--   mempty = []
--   mappend = (++)


---- Product, Sum as Monoids (тут мы делаем числа экземпляром моноида)
{-
instance (Num a) => Monoid (Product a) where
  mempty = Product 1
  mappend (Product x) (Product y) = Product (x * y)

instance (Num a) => Monoid (Sum a) where
  mempty = Product 0
  mappend (Sum x) (Sum y) = Sum (x + y)
-}

---- Any, All as Monoids (тут мы делаем Bool экзмепляром Моноида)
{-
instance Monoid Any where
  mempty = Any False
  mappend (Any x) (Any y) = Any (x || y)
-}

{-
instance Monoid All where
  mempty = All True
  mappend (All x) (All y) = All (x && y)
-}

---- Ordering as Monoid
{-
instance Monoid Ordering where
  mempty = EQ
  mappend LT _ = LT
  mappend EQ y = y
  mappend GT _ = GT
-}

lengthCompare :: String -> String -> Ordering
-- lengthCompare x y = let a = compare (length x) (length y)
--                         b = compare x y
--                     in if a == EQ then b else a
-- lengthCompare x y = mappend (compare (length x) (length y)) (compare x y)
lengthCompare x y = (length x `compare` length y) `mappend`
                    (vowels x `compare` vowels y) `mappend`
                    (compare x y)
  where vowels = length . filter ((flip elem) "aeiou")

---- Maybe as Monoids

-- First Last
-- допиши...

--------------------------------------------------------------------------------
-- Foldable - Это то, по чему можно сделать fold

-- F.foldl (+) 2 (Just 10)
-- F.fold (||) True (Just False)

data Tree a = Leaf | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

instance F.Foldable Tree where
  foldMap f Leaf = mempty
  foldMap f (Node x l r) = F.foldMap f l `mappend` f x `mappend` F.foldMap f r

-- для реализации Foldable достаточно реализовать F.foldMap
-- F.foldMap (\x -> [x]) testTree
