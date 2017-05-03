-- map
-- filter
-- zipWith
-- zip
-- flip
-- takeWhile
-- foldl, foldr
-- scanl, scanr

myMax :: (Ord a) => a -> a -> a
myMax x y
  | x >= y = x
  | otherwise = y

multyThree :: (Num a) => a -> a -> a -> a
multyThree x y z = x * y * z

compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred = compare 100

devideByTen :: (Floating a) => a -> a
devideByTen = (/10)
-- Infix operators should be in brackets

isUpperCase :: Char -> Bool
isUpperCase = (`elem` ['A'..'Z'])
-- isUpperCase = flip elem ['A'..'Z']

minusFour :: (Num a) => a -> a
minusFour = subtract 4

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f x y = f y x

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x:xs)
  | f x = x : filter' f xs
  | otherwise = filter' f xs

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
  let smallerSorted = quicksort (filter (<=x) xs)
      biggerSorted = quicksort (filter (>x) xs)
  in smallerSorted ++ [x] ++ biggerSorted

largest :: (Integral a) => a
largest = head (filter p [100000, 99999..1])
  where p x = mod x 3829 == 0

foo :: Integer
foo = sum (takeWhile (<10000) (filter odd (map (^2) [1..])))

chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
  | even n = n:chain (div n 2)
  | odd n = n:chain (3*n + 1)

numLongChains :: Int
numLongChains = length (filter isLong (map chain [1..100]))
  where isLong x = length x > 15


-- lambdas

numLongChains1 :: Int
numLongChains1 = length (filter (\x -> length x > 15) (map chain [1..100]))

addThree :: (Num a) => a -> a -> a -> a
addThree = \x -> \y -> \z -> x + y + z

flip1 :: (a -> b -> c) -> b -> a -> c
flip1 f = \x y -> f y x

mySum :: (Num a) => [a] -> a
-- mySum xs = foldl (\acc x -> acc + x) 0 xs
mySum = foldl (+) 0

myElem :: (Eq a) => a -> [a] -> Bool
myElem x xs = foldl (\acc t -> if x == t then True else acc) False xs
