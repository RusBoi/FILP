import qualified Data.Map as Map

-- Eq, Ord, Enum, Show, Read, Num, Bounded, Integral, Floating
-- id

--------------------------------------------------------------------------------

data TrafficLight = Red | Yellow | Green

-- создаем новый TypeClass
class MyEq a where
  (.==) :: a -> a -> Bool
  (./=) :: a -> a -> Bool
  x .== y = not (x ./= y) -- эта реализация необязательна
  x ./= y = not (x .== y) -- эта реализация обязательна. Тк оператора /= в instance нет

-- делаем тип TrafficLight - реализующим TypeClass MyEq
instance MyEq TrafficLight where
  Red .== Red = True
  Green .== Green = True
  Yellow .== Yellow = True
  _ .== _ = False

instance Show TrafficLight where
  show Red = "Red light"
  show Green = "Green light"
  show Yellow = "Yellow light"

-- So "class" is for defining new typeclasses and "instance" is for making our types instances of typeclasses.

--------------------------------------------------------------------------------

{-
также Haskell поддерживает наследование в определении typeclass'ов
class (Eq a) => Num a where
  ...
Тут мы гарантировали что тип a должен быть экземпляром typeclass'а Eq
То же самое можно делать и в instance
-}

infixr 5 :-:
data List a = Empty | a :-: (List a)

instance (Show a) => Show (List a) where
  show Empty = ""
  show (x :-: xs) = show x ++ " | " ++ show xs

--------------------------------------------------------------------------------

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
-- тут функтор требует уже не тип (как в примерах раньше), а уже именно type constructor (именно поэтому он и называется Functor)

class MyFunctor f where
  myFmap :: (a -> b) -> f a -> f b

instance MyFunctor [] where
  myFmap = map

-- [a] - concrete type - can't use in instance declaration (because type constructor needed)
-- [] - type constructor - can use in instance declaration

instance MyFunctor Maybe where
  myFmap func Nothing = Nothing
  myFmap func (Just x) = Just (func x)

--------------------------------------------------------------------------------

data Tree a = Leaf | Node a (Tree a) (Tree a) deriving (Show, Eq)

add :: (Ord a) => Tree a -> a -> Tree a
add Leaf val' = Node val' Leaf Leaf
add (Node val left right) val'
  | val' > val = Node val left (add right val')
  | val' < val = Node val (add left val') right
  | otherwise = Node val' left right

treeElem :: (Ord a) => Tree a -> a -> Bool
treeElem Leaf _ = False
treeElem (Node val left right) val'
  | val' > val = treeElem right val'
  | val' < val = treeElem left val'
  | otherwise = True

instance MyFunctor Tree where
  myFmap func Leaf = Leaf
  myFmap func (Node val left right) = Node (func val) (myFmap func left) (myFmap func right)

instance MyFunctor (Either z) where
  myFmap func (Left x) = Left x
  myFmap func (Right x) = Right (func x)

-- так. Фанктору нужен type constructor, который принимает 1 парметр.
-- Either берет 2 парметра, значит мы дадим ему только один (Either a), а второй скормит сам myFmap

instance MyFunctor (Map.Map k) where
  myFmap func list = Map.map func list

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

data Barry t k p = Barry {yabba :: p, dabba :: t k}
-- k = *
-- t = * -> *
-- p = *
-- Barry (* -> *) -> * -> * -> *

instance Functor (Barry a b) where
  fmap f (Barry {yabba = x, dabba = y}) = Barry {yabba = f x, dabba = y}
-- ну що тут можна сказати. Haskell це пиздец
