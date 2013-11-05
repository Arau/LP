import Data.Char (isAlphaNum)

obtenir2llista [] = [] 
obtenir2llista (x:xs) = ((take k xs):obtenir2llista (drop k xs))
			where k = (max 0 (read x)) 

{- noteu que amb el read faig que algun elements de la llista es
   tractin com int i altres com string -}

getWord:: Char -> IO String
getWord m = do c <- getChar
	       if c==m 
	       then return [c]
	       else if not (isAlphaNum c) && c/='-' 
	       	    then return []
	            else do 
	            w <- getWord m
	            return (c:w)

readall:: Char -> IO [String]
readall m = do w <- getWord m
       	       if null w
	       then do
	       l <- readall m 
	       return l
	       else if (last w) == m 
	            then return [(init w)]
	            else do
		    lw <- readall m 
		    return (w:lw)

main  :: IO ()
main  =  do putStrLn "Introdeix la marca i la seqüència: "
      	    m <- getChar
	    l <- readall m
	    putStrLn ""
	    print l
	    putStrLn ""
	    print (obtenir2llista l)
	    putStrLn ""

-- . -1 3 hola com va ,, ! 2 van dos. 
-- .-1 3 hola com va ,, ! 2 van dos. 
-- .-1 3 hola com va ,, ! 2 van dos . 