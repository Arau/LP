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


sizeTree :: Arbre a -> Int
sizeTree Abuit = 0
sizeTree (Node _ l r)  = 1 + (sizeTree l) + (sizeTree r)

-- Ord

instance (Ord a) => Ord (Arbre a) where
    compare Abuit Abuit = EQ
    compare Abuit _     = LT
    compare _     Abuit = GT
    compare t1@(Node x l1 r1) t2@(Node y l2 r2) = do
        if s1 < s2 then
            LT
        else 
            if s1 > s2 then
                GT
            else 
                if x < y then
                    LT
                else 
                    if x > y then
                        GT
                    else
                        if sl1 < sl2 then
                            LT
                        else 
                            if sl1 > sl2 then
                                GT
                             else 
                                 if sr1 < sr2 then
                                     LT
                                 else 
                                     if sr1 > sr2 then
                                         GT
                                     else 
                                         EQ
        where
            s1 = sizeTree t1
            s2 = sizeTree t2
            sl1= sizeTree l1
            sl2= sizeTree l2
            sr1= sizeTree r1
            sr2= sizeTree r2
