maximumR :: (Ord a) => [a] -> a
maximumR [] = error "maximum of empty list"
maximumR [x] = x
maximumR (x:xs)
  | x > maxTail = x
  | otherwise = maxTail
  where maxTail = maximumR xs

maximumM :: (Ord a) => [a] -> a
maximumM [] = error "maximum of empty list"
maximumM [x] = x
maximumM (x:xs) = max x (maximum xs)

replicateR :: (Num i, Ord i) => i -> a -> [a]
replicateR n x
  | n <= 0 = []
  | otherwise = x:replicateR (n - 1) x

takeR :: (Num i, Ord i) => i -> [a] -> [a]
takeR n _
  | n <= 0    = []
takeR _ []    = []
takeR n (x:xs) = x : takeR (n-1) xs

reverseR :: [a] -> [a]
reverseR [] = []
reverseR (x:xs) = reverseR xs ++ [x]

--infinite list!!
repeatR :: a -> [a]
repeatR x = x:repeatR x

zipR :: [a] -> [b] -> [(a,b)]
zipR _ [] = []
zipR [] _ = []
zipR (x:xs) (y:ys) = (x,y):zipR xs ys

elemR :: (Eq a) => a -> [a] -> Bool
elemR _ [] = False
elemR a (x:xs)
  | a == x    = True
  | otherwise = a `elemR` xs

-- quicksort!
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
  let smallerSorted = quicksort [a | a <- xs, a <= x]
      biggerSorted  = quicksort [a | a <- xs, a > x]
  in  smallerSorted ++ [x] ++ biggerSorted

y :: Int
y = y + 1
