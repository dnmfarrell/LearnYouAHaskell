{- rewrite with fmap

  main = do line <- getLine
            let line' = reverse line
            putStrLn $ "You said " ++ line' ++ " backwards!"
            putStrLn $ "Yes, you really said " ++ line' ++ " backwards!"
-}

main = do line <- fmap reverse getLine
          putStrLn $ "You said " ++ line ++ " backwards!"
          putStrLn $ "Yes, you really said " ++ line ++ " backwards!"

-- functor laws
-- 1. fmap id = id
-- 2. fmap (f . g) = fmap f . fmap g
