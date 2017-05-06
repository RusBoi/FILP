import qualified Data.Map as Map

-- -- Eq, Ord, Enum, Show, Read, Num, Bounded, Integral, Floating
--
-- class Eq a where
--   (==) :: a -> a -> Bool
--   (/=) :: a -> a -> Bool
--   -- x == y = not (x Main./= y) -- все равно она заменится тем что написано в instance
--   x /= y = not (x Main.== y)
--
-- data TrafficLight = Red | Yellow | Green
--
-- instance Main.Eq TrafficLight where
--   Red == Red = True
--   Green == Green = True
--   Yellow == Yellow = True
--   _ == _ = False
--
-- -- So "class" is for defining new typeclasses and "instance" is for making our types instances of typeclasses.
--
-- instance Show TrafficLight where
--   show Red = "Red light"
--   show Green = "Green light"
--   show Yellow = "Yellow light"
--
-- -- также Haskell поддерживает наследование в определении typeclass'ов
-- -- class (Eq a) => Num a where
-- --   ...
--
-- infixr 5 :-:
-- data List a = Empty | a :-: (List a)
--
-- instance (Show a) => Show (List a) where
--   show Empty = "Empty list!"
--   show (x :-: xs) = show x ++ "|" ++ show xs

--------------------------------------------------------------------------------
data TrafficLight = Red | Yellow | Green deriving (Eq, Show)

class YesNo a where
  yesno :: a -> Bool

instance YesNo Int where
  yesno val = val /= 0

instance YesNo [a] where
  yesno [] = False
  yesno _ = True

-- id - функция кторая принимает параметр и возвращает его же
instance YesNo Bool where
  yesno = id

instance YesNo (Maybe a) where
  yesno Nothing = False
  yesno (Just _) = True

instance YesNo TrafficLight where
  yesno Red = False
  yesno _ = True

yesnoif :: (YesNo y) => y -> a -> a -> a
yesnoif yesnoval yesres nores = if yesno yesnoval
                                then yesres
                                else nores
--------------------------------------------------------------------------------
-- Functor typeclass
-- тут фнктор требует уже не тип (как в примерах раньше), а уже именно type constructor (именно поэтому он и называется Functor)

class Functor' f where
  fmap' :: (a -> b) -> f a -> f b

instance Functor' [] where
  fmap' = map

-- [a] - concrete type - can't use in instance declaration (because type constructor needed)
-- [] - type constructor - can use in instance declaration

instance Functor' Maybe where
  fmap' f Nothing = Nothing
  fmap' f (Just x) = Just (f x)

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Eq)

add :: (Ord a) => Tree a -> a -> Tree a
add Empty val' = Node val' Empty Empty
add (Node val left right) val'
  | val' > val = Node val left (add right val')
  | val' < val = Node val (add left val') right
  | otherwise = Node val' left right

treeElem :: (Ord a) => Tree a -> a -> Bool
treeElem Empty _ = False
treeElem (Node val left right) val'
  | val' > val = treeElem right val'
  | val' < val = treeElem left val'
  | otherwise = True

instance Functor' Tree where
  fmap' f Empty = Empty
  fmap' f (Node val left right) = Node (f val) (fmap' f left) (fmap' f right)

instance Functor' (Either z) where
  fmap' f (Left x) = Left x
  fmap' f (Right x) = Right (f x)
-- так. Фанктору нужен type constructor, который принимает 1 парметр.
-- Either берет 2 парметра, значит мы дадим ему только один (Either a)б а второй скормит сам fmap'


instance Functor' (Map.Map k) where
  fmap' f list = Map.map f list
--------------------------------------------------------------------------------

class ToFu t where
  tofu :: j a -> t a j

-- a = *
-- j = * -> *
-- t = a -> j -> * = * -> (* -> *) -> *

data Frank a b = Frank {frankField :: b a} deriving (Show)
-- a = *
-- b = * -> *
-- Frank = * -> (* -> *) -> *

instance ToFu Frank where
  tofu x = Frank x

data Barry t k p = Barry {yabba :: p, dabba ::  t k}
-- k = *
-- t = * -> *
-- p = *
-- Barry (* -> *) -> * -> * -> *

instance Functor (Barry a b) where
  fmap f (Barry {yabba = x, dabba = y}) = Barry {yabba = f x, dabba = y} -- ну що тут можна сказати. Haskell це пиздец
