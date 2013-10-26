-- Max d'una llista d'enters

my_max [x] = x
my_max (x:xs)
    | x > y     = x
    | otherwise = y
    where y = my_max(xs)

-- Avg of list
my_avg []   = 0
my_avg [x]  = x
my_avg  l   = my_sum l `div` my_count l

my_sum [x]      = x
my_sum (x:xs)   = x + my_sum xs

my_count []     = 0
my_count (x:xs) = 1 + my_count(xs)


my_avg2 []  = 0

my_avg2 [x] = x
my_avg2 l   = aux_avg2 0 0 l

aux_avg2 n  added     []    | n >  0    = added `div` n
aux_avg2 n  value   (x:xs)  | n >= 0    = aux_avg2 count added xs
    where   count = n + 1
            added = x + value


-- Concat lists

flat [] = []
flat (x:xs) = x ++ flat(xs)

-- T
