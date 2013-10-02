-- Max d'una llista d'enters

my_max [x] = x
my_max (x:xs)
    | x > y     = x
    | otherwise = y
    where y = my_max(xs)

my_avg []   = 0
my_avg [x]  = x
my_avg  l   = my_sum l `div` my_count l

my_sum [x]      = x
my_sum (x:xs)   = x + my_sum xs

my_count []     = 0
my_count (x:xs) = 1 + my_count(xs)
