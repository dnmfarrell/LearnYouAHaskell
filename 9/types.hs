type FirstName = String
type LastName  = String
type Age = Int

data Person = Person FirstName LastName Age deriving (Eq, Show)

eldest :: Person -> Person -> String
eldest (Person x1 y1 z1) (Person x2 y2 z2)
  | z1 > z2   = x1 ++ " " ++ y1 ++ " is older"
  | z1 < z2   = x2 ++ " " ++ y2 ++ " is older"
  | otherwise = "They're the same age!"

initials :: Person -> String
initials (Person x y _) = [head x,'.',head y, '.']

quicksort [] = []
quicksort (x:xs) = 
    let smallerSorted = quicksort [a | a <- xs, a <= x]
        biggerSorted = quicksort [a | a <- xs, a > x]
    in  smallerSorted ++ [x] ++ biggerSorted

instance Ord Person where
  compare (Person _ a _) (Person _ b _) = compare a b
