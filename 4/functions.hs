bmiTell :: (RealFloat a) => a -> a -> String
bmiTell w h
  | bmi <= underweight = "You're underweight, eat up!"
  | bmi <= normal      = "You're supposedly normal. Great job!"
  | bmi <= overweight  = "You could lose some weight"
  | otherwise          = "Stop eating right now!"
  where bmi = w / h ^ 2
        underweight = 18.5
        normal      = 25.0
        overweight  = 30.0

bmiCalc :: (RealFloat a) => a -> a -> a
bmiCalc w h = w / h ^ 2

max' :: (Ord a) => a -> a -> a
max' a b | a > b = a | otherwise = b

myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
  | a >  b    = GT
  | a == b    = EQ
  | otherwise = LT

initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
  where (f:_) = firstname
        (l:_) = lastname

calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi w h | (w, h) <-xs]
  where bmi weight height = weight / height ^ 2

calcBmisFatOnly :: (RealFloat a) => [(a, a)] -> [a]
calcBmisFatOnly xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2, bmi > 25.0]

cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
  let sideArea = 2 * pi * r * h
      topArea  = pi * r ^2
  in sideArea + 2 * topArea

headPattern :: [a] -> a
headPattern [] = error "No head for empty lists!"
headPattern (x:_) = x

headCase :: [a] -> a
headCase xs = case xs of []    -> error "No head for empty lists!"
                         (x:_) -> x

describeList :: [a] -> String
describeList xs = "The list is " ++ case xs of [] -> "empty."
                                               [x]-> "a singleton list."
                                               xs -> "a longer list."

describeListWhere :: [a] -> String
describeListWhere xs = "The list is " ++ what xs
  where what [] = "empty."
        what [x]= "a singleton list."
        what xs = "a longer list."
