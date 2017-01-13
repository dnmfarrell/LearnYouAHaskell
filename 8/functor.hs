-- Functor is a typeclass like Ord, Eq, Show. It defines things that can be
-- mapped (like Maybe a | Maybe | Just a).
--
-- We can add the Functor typeclass to a Tree type:
{-
  instance Functor Tree where
      fmap f EmptyTree = EmptyTree
      fmap f (Node x leftsub rightsub) = Node (f x) (fmap f leftsub) (fmap f rightsub)
-}

-- Now we can use fmap on Trees:
-- fmap (*2) (foldr treeInsert EmptyTree [5,7,3,2,1,7])

-- Either a is a functor in the sandard libraries:
{-
  instance Functor (Either a) where
    fmap f (Right x) = Right (f x)
    fmap f (Left x)  = Left x
-}

-- types have their own labels, called "kinds"
-- a kind is like the type of a type
-- the ":k" command in GHCI displays the kind

class Tofu t where
  tofu :: j a -> t a j

data Frank a b = Frank {frankField :: b a} deriving (Show)

instance Tofu Frank where
  tofu x = Frank x

data Barry t k p = Barry { yabba :: p dabba :: t k }

instance Functor (Barry a b) where
  fmap f (Barry {yabba = x, dabba = y}) = Barry {yabba = f, dabba = y}


