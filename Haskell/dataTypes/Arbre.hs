module Arbre where

{-- Arbres binaris amb arbres buits --}
data Arbre a = Node a (Arbre a) (Arbre a)
             | Abuit
 deriving (Eq,Show)

abuit = Abuit

plantar x a1 a2 = Node x a1 a2

arrel (Node x _ _) = x

fe (Node _ a1 _) = a1
fd (Node _ _ a2) = a2

esbuit Abuit = True
esbuit (Node _ _ _) = False

