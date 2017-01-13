-- type classes 102

-- data is for making new data types
data TrafficLight = Red | Yellow | Green

-- instance is for making a type an instance of a typeclass
instance Eq TrafficLight where
  Red    == Red = True
  Green  == Green = True
  Yellow == Yellow = True
  _==_ = False

-- class is for defining new typeclasses (traits)
-- E.g. we could re-implement Eq like this:
{-
  class Eq a where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool
    x == y = not (x /=)
    x /= y = not (x == y)
-}

instance Show TrafficLight where
  show Red    = "Red light"
  show Yellow = "Yellow light"
  show Green  = "Green light"

-- You can also make typeclasses that are subclasses of other typeclasses!!!
-- Just add a type constraint to class e.g. make Num a subclass of Eq:
{-
  class (Eq a) => Num a where
  ...
-}

-- instances can also have constraints. These are used to express requirements
-- about the contents of a type:
{-
  instance (Eq m) => Eq (Maybe m) where
  ...
-}

-- GHCI info command
-- info TypeClassName will show what instances of a typeclass there are
-- info functionName will show you the type declaration of a function

-- Implement weakly-typed boolean behavior

class YesNo a where
  yesno :: a -> Bool

instance YesNo Int where
  yesno 0 = False
  yesno 1 = True

instance YesNo [a] where
  yesno [] = False
  yesno _  = True

-- "id" takes a param and returns the same thing
instance YesNo Bool where
  yesno = id

instance YesNo (Maybe a) where
  yesno (Just _) = True
  yesno Nothing  = False

instance YesNo TrafficLight where
  yesno Red = False
  yesno _   = True

yesnoIf :: (YesNo y) => y -> a -> a -> a
yesnoIf yesnoVal yesResult noResult = if yesno yesnoVal then yesResult else noResult

