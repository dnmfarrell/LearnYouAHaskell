-- Only print lines shorter than 10 characters

{- Could define main like this, but interact is equivalent

  main = do
    contents <- getContents
    putStr $ shortLinesOnly contents
-}

main = interact shortLinesOnly

shortLinesOnly :: String -> String
shortLinesOnly input =
  let allLines = lines input
      shortLines = filter (\line -> length line < 10) allLines
      result = unlines shortLines
  in result

-- this whole program can be a one liner
-- main = intereact $ unLines . filter ((<10) . length) .lines
