import System.IO

main = do
  handle <- openFile "girlfriend" ReadMode
  contents <- hGetContents handle
  putStr contents
  hClose handle

{- we could also write this using "getFile"

  main = do
    withFile "girlfriend" ReadMode (\handle -> do
        contents <- hGetContents handle
        putStr contents)

Or readFile

  main = do
    contents = <- readFile "girlfriend"
    putStr contents

-}
