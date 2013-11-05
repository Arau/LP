import Arbre

obtenirArbre (x:xs) m = if (x==m) then (abuit,xs)
	                else ((plantar x a1 a2),lr)
			where 
			  (a1,l)= (obtenirArbre xs m) 
			  (a2,lr)= (obtenirArbre l m) 

mostrarArbre Abuit m = [m]
mostrarArbre (Node x a1 a2) m = x:(mostrarArbre a1 m)++(mostrarArbre a2 m)

main  :: IO ()
main  =  do putStrLn "Introdueix l'arbre amb 0 a l'arbre buit: "
      	    m <- getLine
            print(mostrarArbre (fst (obtenirArbre (getAllInt (words m)) 0)) 0)

-- getLine :: IO (String) 
-- llegeix una línia de l'entrada.
 
-- words :: String -> [String]
-- es predefinida i separa un string en paraules 
	   
getAllInt []  =  []
getAllInt (x:xs)  =  (read x):(getAllInt xs)

-- read converteix un string en el tipus esperat (si té el format correcte)

-- exemple d'entrada: 
-- 3 2 0 0 4 5 0 0 0

