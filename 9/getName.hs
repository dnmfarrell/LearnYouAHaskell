import Data.Char

main = do
  putStrLn "Hello, what's your first name?"
  firstName <- getLine
  putStrLn "Hello, what's your last name?"
  lastName <- getLine
  let bigFirstName = map toUpper firstName
      bigLastName  = map toUpper lastName
  putStrLn $ "hey " ++ bigFirstName ++ " " ++ bigLastName ++ ", how are you?"

tellFortune :: String -> String
tellFortune x = x ++ ", you will live forever!!"
