import Data.List

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub -- from Data.List, remove dups

-- import methods
-- import module -- all methods
-- import module (func1, func2)
-- import module hiding (func1, func2) -- all except these funcs
-- import qualified module -- keep function within module namespace
-- import qualified module as M -- rename namespace to M

search :: (Eq a) => [a] -> [a] -> Bool
search needle haystack =
  let nlen = length needle
  in  foldl (\acc x -> if take nlen x == needle then True else acc) False (tails haystack)

-- functions to check for list within another list:
-- isPrefixOf, isInfixOf, isSuffixOf 

-- functions to separate lists:
-- span, break, partition
-- they accept predicate conditions and return 2 lists
-- allocated based on the predicate

-- A maybe value can be either an element or a empty value
--
