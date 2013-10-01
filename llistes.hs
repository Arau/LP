-- Max d'una llista d'enters

my_max [x] = x
my_max (x:xs)
    | x > y     = x
    | otherwise = y
    where y = my_max(xs)

