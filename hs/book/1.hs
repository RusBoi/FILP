doubleMe x = 2 * x
doubleUs x y = doubleMe x + doubleMe y
doubleSmallNumber x = if x > 100 then x else x * 2

removeCapital :: [Char] -> [Char]
removeCapital xs = [x | x <- xs, elem x ['a'..'z']]

addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

factorial :: (Integral a) => a -> a -- == Integral a -> Integral a
factorial x = product [1..x]

-- Int
-- Integer
-- Float
-- Double
-- Bool
-- Char

-- TypeClasses:
-- Eq
-- Ord
-- Show
-- Read
-- Enum
-- Bounded
-- Num
-- Integral
-- Floating

compare a b
show a b
read a
-- read "5" :: Int
succ a
pred a
maxBound a
-- maxBound :: (Bool, Char, Integer)
minBound a
fromIntegral
-- fromIntegral (length [1,2,3,4]) + 2.7
