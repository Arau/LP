module Arbre where

{-- Arbres binaris amb arbres buits --}
data Arbre a = Node a (Arbre a) (Arbre a)
             | Abuit
 deriving (Eq)

abuit = Abuit

plantar x a1 a2 = Node x a1 a2

arrel (Node x _ _) = x

fe (Node _ a1 _) = a1
fd (Node _ _ a2) = a2

esbuit Abuit = True
esbuit (Node _ _ _) = False

-- Show

instance Show a => Show (Arbre a) where
    show tree = sTree tree 0

sTree :: (Show a, Eq n, Num n) => Arbre a -> n -> String
sTree  Abuit lvl = (tab lvl) ++ "_"
sTree (Node x chl chr)  lvl = t ++ (show  x)
                                ++ form ++ (sTree chl i)
                                ++ form ++ (sTree chr i)
                            where 
                                i = sum [lvl,1]
                                t = (tab lvl)
                                form = "\n" ++ t

tab :: (Eq a, Num a) => a -> [Char]
tab 0 = ""
tab n = "  " ++ (tab y)
    where y = subtract 1 n
