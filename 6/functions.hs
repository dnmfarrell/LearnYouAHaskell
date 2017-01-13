{-# OPTIONS_GHC -Wall #-}
multThree :: (Num a) => a -> a -> a -> a
multThree x y z = x * y * z

compareHundred :: (Num a, Ord a) => a -> Ordering
compareHundred = compare 100

divideByTen :: (Floating a) => a -> a
divideByTen = (/10)

isUpperAlpha :: Char -> Bool
isUpperAlpha = (`elem` ['A'..'Z'])

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

-- rewrite with function application
applyTwiceApp :: (a -> a) -> a -> a
applyTwiceApp f x = f $ f x

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = g
  where g x y = f y x

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x:xs)
  | f x       = x : filter f xs
  | otherwise = filter f xs

-- sum of all odd squares under 10,000
-- sum (takeWhile (<10000) [ x | x <- (map (^2) [1..]), odd x ])

chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
  | even n = n:chain (n `div` 2)
  | otherwise = n:chain(n*3 + 1)

{- emits warnings, disabled for now
numLongChains :: Int
numLongChains = length (filter isLong (map chain [1..100]))
  where isLong xs = length xs > 15

numLongChainsLambda :: Int
numLongChainsLambda = length (filter (\xs -> length xs > 15) (map chain [1..100]))
-}
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
  let smallerSorted = quicksort (filter (<=x) xs)
      biggerSorted  = quicksort (filter (>x) xs)
  in  smallerSorted ++ [x] ++ biggerSorted

-- find first num < 100,000 that cleanly divides by 3829
-- head([x | x <- [100000, 99999..], x `mod` 3829 == 0])
-- implemented as a function
largestDivisible :: (Integral a) => a
largestDivisible = head (filter p [100000,99999..])
  where p x = x `mod` 3829 == 0

-- Example of indexing an infinite list with partial function
-- let listOfFuns = map (*) [0..]
-- (listOfFuns !! 4) 5
-- 20

-- Examples using left/right fold

sumFold :: (Num a) => [a] -> a
sumFold xs = foldl (\acc x -> acc + x) 0 xs

sumFoldIdiomatic :: (Num a) => [a] -> a
sumFoldIdiomatic = foldl (+) 0

elemFold :: (Eq a) => a -> [a] -> Bool
elemFold y ys = foldl (\acc x -> if x == y then True else acc) False ys

mapFoldr :: (a -> b) -> [a] -> [b]
mapFoldr f xs = foldr (\x acc -> f x : acc) [] xs

mapFoldl :: (a -> b) -> [a] -> [b]
mapFoldl f xs = foldl (\acc x -> acc ++ [f x]) [] xs

--STDLIB functions implemented with folds
--foldl1 and foldr1 use the first element as the starting value
--empty lists can cause runtime errors though!

maximumFold :: (Ord a) => [a] -> a
maximumFold = foldr1 (\x acc -> if x > acc then x else acc)

reverseFold :: [a] -> [a]
reverseFold = foldl (\acc x -> x : acc) []

productFold :: (Num a) => [a] -> a
productFold = foldr1(*)

filterFold :: (a -> Bool) -> [a] -> [a]
filterFold p = foldr (\x acc -> if p x then x : acc else acc) []


-- have to use right fold here to roll up the list
-- from the right-side and return the "last" element
-- which is the first element :)
headFold :: [a] -> a
headFold = foldr1 (\x _ -> x)

-- Ditto here to get the last element, roll up the 
-- the from the left-side, and the last element
-- will be last
lastFold :: [a] -> a
lastFold = foldl1 (\_ x -> x)

-- scanl and scanr are like map in that they return a list
-- but they maintain an accumulator the whole way through
--
-- Count how many elements it takes for the root of all
-- natural numbers to exceed 1000
sqrtSums :: Int
sqrtSums = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) + 1

-- $ is the function application it returns whatever value
-- it is given. It has very low precedence and can be used
-- to reduce use of parentheses
-- sum (map sqrt [1..130])
-- becomes:
-- sum $ map sqrt [1..130]
-- and:
-- sum (filter (>10) (map (*2) [2..10]))
-- becomes:
-- sum $ filter (>10) $ map (*2) [2..10]
--
-- function application can also be passed like a function:
-- map ($ 3) [(4+), (10*), (^2)]
-- [7, 30, 9]
--
-- function composition is done with "."
-- map (\x -> negate (abs x)) [5, -3, -6, -7]
-- becomes:
-- map (negate . abs) [5,-3,-6,7]
--
-- Can also be used for functions that take multiple params:
-- sum (replicate 5 (max 6.7 6.9))
-- sum . replicate 5 . max 6.7 $ 8.9
-- or:
-- replicate 100 (product (map (*3) (zipWith max [1,2,3,4,5] [4,5,6,7,8])))
-- replicate 100 . product . map (*3) . zipWith max [1,2,3,4,5] $ [4,5,6,7,8]
--
-- Pro TIP! If the function ends with 3 parentheses, it probably needs 3 composition
-- operators
--
-- free point style involves rewriting functions to remove explicit params
-- and rely on currying e.g.
sumAlt :: (Num a) => [a] -> a
sumAlt xs = foldl (+) 0 xs
-- becomes
sumFreePoint :: (Num a) => [a] -> a
sumFreePoint = foldl (+) 0

-- fn x = ceiling (negate (tan (cos (max 50 x))))
-- becomes:
-- fn = celing . negate . tan . cos . max 50
--
-- Three ways of writing a function, oddSquareSUm

oddSqSum :: Integer
oddSqSum = sum (takeWhile (<10000) (filter odd (map (^2) [1..])))

oddSqSumComp :: Integer
oddSqSumComp = sum . takeWhile (<10000) . filter odd . map (^2) $ [1..]

oddSqSumLet :: Integer
oddSqSumLet =
  let oddSquares = filter odd $ map (^2) [1..]
      belowLimit = takeWhile (<10000) oddSquares
  in  sum belowLimit
