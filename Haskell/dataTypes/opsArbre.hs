import Arbre

obtenirArbre :: Eq a => [a] -> a -> (Arbre a, [a])
-- crea un arbre d'element de tipus a a partir d'una llista de tipus a
-- i un element de tipus a que indica on hi ha arbres buits
-- retorna l'arbre i la resta de la llista

obtenirArbre (x:xs) m = if (x==m) then (abuit,xs)
	                else ((plantar x a1 a2),lr)
			where 
			  (a1,l)= (obtenirArbre xs m) 
			  (a2,lr)= (obtenirArbre l m) 

mostrarArbre Abuit m = [m]
mostrarArbre (Node x a1 a2) m = x:(mostrarArbre a1 m)++(mostrarArbre a2 m)

prova  = (mostrarArbre (fst (obtenirArbre [3,2,0,0,4,5,0,0,0] 0)) 0)


myTree = fst (obtenirArbre [3,2,0,0,4,5,0,0,0] 0)

highTree :: Arbre a -> Int
highTree Abuit = 0
highTree tree  = 1 + max (highTree $ fe tree) (highTree $ fd tree)

cmpTree :: Eq a => Arbre a -> Arbre a -> Bool
cmpTree Abuit Abuit = True
cmpTree _     Abuit = False
cmpTree Abuit _     = False
cmpTree (Node x cl1 cr1) (Node y cl2 cr2) = x==y && (cmpTree cl1 cl2) && (cmpTree cr1 cr2)
