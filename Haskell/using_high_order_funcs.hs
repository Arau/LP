-- Compare lists

equal_lists :: Eq a => [a] -> [a] -> Bool
equal_lists [] [] = True
equal_lists [] _  = False
equal_lists _  [] = False
equal_lists (x:xs) (y:ys)
    | x /= y    = False
    | otherwise = equal_lists xs ys


-- Prod elements of lists

prod_list :: Num a => [a] -> a
prod_list []     = 0
prod_list l1     = foldr (*) 1 l1


-- Prod even numbers of list

prod_even list = reduce_prod $ filter (even) list

reduce_prod []      = 1
reduce_prod (x:xs)  = x*reduce_prod xs


-- Scalar prod between 2 lists

prod_scalar l1  [] = l1
prod_scalar []  l2 = l2
prod_scalar l1  l2 = zipWith (*) l1 l2


-- High order func which return number of
-- elems that satisfy a condition

countIf _   []  = 0
countIf f   lst = length $ filter f lst
