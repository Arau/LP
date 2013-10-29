-- Compare lists

equal_lists :: Eq a => [a] -> [a] -> Bool
equal_lists [] [] = True
equal_lists [] _  = False
equal_lists _  [] = False
equal_lists (x:xs) (y:ys)
    | x /= y    = False
    | otherwise = equal_lists xs ys

