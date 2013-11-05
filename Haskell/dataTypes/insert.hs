
main  =  do putStrLn "Quantes elemnts hi ha: "
      	    n <- getLine
	    putStrLn "Introdueix els elements ordenats creixentment en linies diferents: "
      	    l <- getAll (read n)
	    putStrLn "Introdueix l'element a inserir: "
      	    m <- getLine
            print (inserir (read m) l)
	   
getAll     :: Int -> IO [Int]
getAll n   =  do if n == 0
                 then return []
                 else do
		 l <- getLine 
		 al <- getAll (n-1)
                 return ((read l):al)

inserir m [] = [m]
inserir m (x:xs) 
	| m < x      = (m:x:xs)
	| otherwise  = x:(inserir m xs) 
