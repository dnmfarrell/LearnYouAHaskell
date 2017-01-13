-- sequence takes a list of IO actions and returns IO actions that will perform those actions one after another

{- this:

  main = do
    a <- getLine
    b <- getLine
    c <- getLine
    print [a,b,c]

is the same as:-}

main = do
  rs <- sequence [getLine, getLine, getLine]
  print rs

-- A common use of of sequence is creating IO actions:
-- sequence (map print [1..5])
