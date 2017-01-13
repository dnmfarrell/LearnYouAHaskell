-- this is record syntax
-- it automatically creates accessor functions 
-- for the sub types
data Person = Person {
  firstName :: String,
  lastName :: String,
  age :: Int,
  height :: Float,
  phoneNumber :: String,
  flavor :: String 
} deriving (Show)

data Car = Car {
  company :: String,
  model :: String,
  year :: Int
} deriving (Show)

tellCar :: Car -> String
tellCar (Car {company = c, model = m, year = y}) = "This " ++ c ++ " " ++ m ++ " was made in " ++ show y

-- Type parameters are args for constructors to produce new types
data Maybe' a = Nothing | Just a

-- Haskell best practice: do NOT use typeclass constraints in data declarations
-- this reduces the need to add type constraints to every function declaration
-- using that data

