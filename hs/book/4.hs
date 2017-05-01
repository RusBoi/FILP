-- Syntax in Functions


lucky :: (Integral a) => a -> String
lucky 7 = "You guessed it!"
lucky 7 = "You guessed it! (2)"
lucky x = "Try again, don't worry about that :)"

factorial :: (Integral a) => a -> a
-- factorial n = product [1..n]

factorial 0 = 1
factorial n = n * factorial (n - 1)

vAdd :: (Num a) => (a, a) -> (a, a) -> (a, a)
-- vAdd a b = (fst a + fst b, snd a + snd b)
vAdd (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

first :: (Num a) => (a, a, a) -> a
first (x, _, _) = x

two :: (Num a) => [a] -> a
two (x:y:_) = x + y

len :: (Num b) => [a] -> b
-- len xs = sum [1 | _ <- xs]
len [] = 0
len (x:xs) = 1 + len xs

mySum :: (Num a) => [a] -> a
mySum [] = 0
mySum (x:xs) = x + mySum xs

capital :: String -> String
capital "" = "Empty String!"
capital s@(x:_) = "The first letter of '" ++ s ++ "' is " ++ [x]

ageTell :: (Int) -> String
ageTell age
  | lol <= 18 = "You are fucking kid, die!"
  | lol <= 30 = "You are a normal human being"
  | otherwise = "Hi, grandpa!"
  where lol = age * 10

max' :: (Ord a) => a -> a -> a
max' a b
  | a >= b = a
  | otherwise = b

compare' :: (Ord a) => a -> a -> Ordering
compare' a b
  | a > b = GT
  | a < b = LT
  | otherwise = EQ

bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
    | bmi <= skinny = "You're underweight, you emo, you!"
    | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"
    | bmi <= fat = "You're fat! Lose some weight, fatty!"
    | otherwise = "You're a whale, congratulations!"
    where bmi = weight / height ^ 2
          (skinny, normal, fat) = (18.5, 25.0, 30.0)

initials :: String -> String -> String
initials firstName lastName = [f] ++  ". " ++ [l] ++ "."
  where (f:_) = firstName
        (l:_) = lastName

-- calcBmis :: (RealFloat a) => [(a, a)] -> [a]
-- calcBmis xs = [bmi w h | (w, h) <- xs]
--   where bmi weight height = weight / height ^ 2
--  тут в блоке where мы определили целую функцию

cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
  let sideArea = 2 * pi * r * h
      topArea = pi * r ^ 2
  in sideArea + 2 * topArea

-- разница между let и where в том, что let - выражение и where  - это всего лишь синтаксический сахар. Выражения можно вставлять везде
-- let xs = [if 10 > 5 then 1 else 2, if 5 > 5 then 1 else 2]

calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2, bmi >= 25.0]

head' :: [a] -> a
head' [] = error "List is empty!"
head' (x:_) = x

head1 :: [a] -> a
head1 xs = case xs of [] -> error "No head for empty lists!"
                      (x:_) -> x

-- case expression of pattern -> result
--                    pattern -> result
--                    pattern -> result
--                    ...


describeList :: [a] -> String
describeList xs = "The list is " ++ case xs of [] -> "empty."
                                               [x] -> "a singleton list."
                                               xs -> "a longer list."

-- describeList :: [a] -> String
-- describeList xs = "The list is " ++ what xs
--    where what [] = "empty."
--          what [x] = "a singleton list."
--          what xs = "a longer list."
