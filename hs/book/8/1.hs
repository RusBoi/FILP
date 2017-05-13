-- Maybe a
-- Either a
-- infixr
-- infixl

module Shapes (Point(..), Shape (..), surface) where
import qualified Data.Map as Map

data Point = Point Float Float deriving (Show, Eq)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show, Eq)

surface :: Shape -> Float
surface (Circle _ r) = pi * r ^ 2
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x1 - x2) * (abs $ y1 - y2)

nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) a b = Circle (Point (x+a) (y+b)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b = Rectangle (Point (x1+a) (y1+b)) (Point (x2+a) (y2+b))

baseCircle :: Float -> Shape
baseCircle r = Circle (Point 0 0) r

baseRect :: Float -> Float -> Shape
baseRect width height = Rectangle (Point 0 0) (Point width height)

--------------------------------------------------------------------------------

-- data Person = Person String String Int Float
data Person = Person {firstName :: String, lastName :: String, age :: Int, height :: Float} deriving (Show, Eq, Read)

tellPerson :: Person -> String
tellPerson (Person name1 name2 age height) = "His name is " ++ name1

-- data Maybe a = Nothing | Just a deriving (Show)
-- data Either a b = Left a | Right b deriving (Eq, Ord, Show, Read)

--------------------------------------------------------------------------------

data Vector a = Vector a a a deriving (Show)

vplus :: (Num t) => Vector t -> Vector t -> Vector t
vplus (Vector x1 y1 z1) (Vector x2 y2 z2) = Vector (x1+x2) (y1+y2) (z1+z2)

smult :: (Num t) => Vector t -> Vector t -> t
smult (Vector x1 y1 z1) (Vector x2 y2 z2) = x1*x2 + y1*y2 + z1*z2


data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
  deriving (Eq, Ord, Read, Show, Bounded, Enum)

--------------------------------------------------------------------------------

type Model = String
type Year = Int
type Price = Int

printCar :: Model -> Year -> Price -> String
printCar m _ _ = "You just biught a " ++ m


-- type AssocList k v = [(k, v)] -- ассоциативный список
-- get :: (Eq k) -> AssocList k v -> k -> Maybe v


-- type IntMap v = Map.Map Int v
-- type IntMap = Map.Map Int

--------------------------------------------------------------------------------

data LockerState = Taken | Free deriving (Show, Eq)
type Code = String
type LockerMap = Map.Map Int (LockerState, Code)

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber lockerMap =
  case Map.lookup lockerNumber lockerMap of
    Nothing -> Left $ "Locker number " ++ show lockerNumber ++ " doesn't exist!"
    Just (state, code) -> if state /= Taken
                          then Right code
                          else Left $ "Locker " ++ show lockerNumber ++ " already taken!"

lockers :: LockerMap
lockers = Map.fromList
  [(100, (Taken, "pass1")),
   (202, (Free, "pass2")),
   (203, (Free, "pass3")),
   (123, (Taken, "pass4"))]

--------------------------------------------------------------------------------

-- data List a = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord)
-- data List a = Empty | Cons {listHead :: a, listTail :: List a} deriving (Show, Read, Eq, Ord)


-- определим новый оператор
infixr 5 :-:  -- оператор :-:  с приоритетом 5

data List a = Empty | a :-: (List a) deriving (Show, Read, Eq, Ord)

infixr 5 .++  -- оператор .++ с приоритетом 5 (аналог конкатенации)

(.++) :: List a -> List a -> List a
Empty .++ ys = ys
(x :-: xs) .++ ys = x :-: (xs .++ ys)

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
