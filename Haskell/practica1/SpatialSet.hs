module SpatialSet where

import Data.List (sortBy)

    -- Question 1 --
    -- SpatialSet defining a QuadTree
    -- @QuadTree children -> NE, SE, SW, NW
data SpatialSet a  = Node (a,a) (SpatialSet a) (SpatialSet a) (SpatialSet a) (SpatialSet a)
                   | Tvoid


build p ne se sw nw = Node p ne se sw nw

    -- Comparision between 2 SpatialSet
instance (Eq a, Num a, Ord a) => Eq (SpatialSet a) where
    qTree1 == qTree2    = cmp_spatial_set qTree1 qTree2

cmp_spatial_set :: (Eq a, Num a, Ord a) => SpatialSet a -> SpatialSet a -> Bool
cmp_spatial_set (Node _ _ _ _ _)  Tvoid           = False
cmp_spatial_set Tvoid            (Node _ _ _ _ _) = False
cmp_spatial_set Tvoid             Tvoid           = True
cmp_spatial_set t1@(Node p1 ne1 se1 sw1 nw1) t2@(Node p2 ne2 se2 sw2 nw2)
    | l1 /= l2  = False
    | otherwise = True
    where
        l1 = sortBy (\x y -> cmp x y) $ get_all t1
        l2 = sortBy (\x y -> cmp x y) $ get_all t2
        cmp p p' = compare (fst p) (fst p')


    -- Show SpatialSet
instance (Show a, Eq a) =>  Show (SpatialSet a) where
    show tree = sTree tree 0 ""

sTree :: (Show a, Eq a, Eq n, Num n) => SpatialSet a -> n -> String -> String
sTree Tvoid _ _ = ""
sTree (Node p ne se sw nw) i str = str ++ show p
                                ++ (sTree ne inc ne_out)
                                ++ (sTree se inc se_out)
                                ++ (sTree sw inc sw_out)
                                ++ (sTree nw inc nw_out)
                                where
                                    inc  = i + 1
                                    ne_out = "\n" ++ (tab inc) ++ "<NE> "
                                    se_out = "\n" ++ (tab inc) ++ "<SE> "
                                    sw_out = "\n" ++ (tab inc) ++ "<SW> "
                                    nw_out = "\n" ++ (tab inc) ++ "<NW> "

tab :: (Eq a, Num a) => a -> [Char]
tab 0 = ""
tab n = "    " ++ (tab y)
    where y = n - 1


    -- Question 2 --
    -- Insert point in SpatialSet
insert :: (Ord a) => SpatialSet a -> (a, a) -> SpatialSet a
insert Tvoid p = (Node p Tvoid Tvoid Tvoid Tvoid)
insert (Node p ne se sw nw) p' =
    if x < x' then
        if y < y' then
            (Node p (insert ne p') se sw nw)
        else 
            (Node p ne (insert se p') sw nw)
    else
        if y < y' then
            (Node p ne se sw (insert nw p'))
        else 
            (Node p ne se (insert sw p') nw)
    where
        x  = (fst p)
        x' = (fst p')
        y  = (snd p)
        y' = (snd p')



    -- Question 3 --
    -- Get a list of all elements of SpatialSet
get_all :: (Num a) => SpatialSet a -> [(a,a)]
get_all Tvoid   = []
get_all (Node p ne se sw nw) = [p] ++ ( concat $ map (get_all) [ne,se,sw,nw] )
