module SpatialSet where

import Data.List (sortBy)

    -- Question 1 --
    -- SpatialSet defining a QuadTree
    -- @QuadTree children -> NE, SE, SW, NW
data SpatialSet a  = Node (a,a) (SpatialSet a) (SpatialSet a) (SpatialSet a) (SpatialSet a)
                   | Tvoid


plant :: (a, a) -> SpatialSet a
plant p = (Node p Tvoid Tvoid Tvoid Tvoid)

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


    -- 2.2 --
    -- Build a SpatialSet for a given list
build :: Ord a => [(a, a)] -> SpatialSet a
build []     = Tvoid
build (p:ps) = build_with_root root ps
    where
        root = plant p

build_with_root :: Ord a => SpatialSet a -> [(a, a)] -> SpatialSet a
build_with_root tree []     = tree
build_with_root tree (p:ps) = build_with_root (insert tree p) ps

    -- Question 3 --
    -- Get a list of all elements of SpatialSet
get_all :: (Num a) => SpatialSet a -> [(a,a)]
get_all Tvoid   = []
get_all (Node p ne se sw nw) = [p] ++ ( concat $ map (get_all) [ne,se,sw,nw] )


    -- Question 4 --
    -- Remove an element from SpatialSet
remove Tvoid p = Tvoid
remove (Node p ne se sw nw) p' = Tvoid -- To implement


    -- Question 5 --
    -- Search and element
contains :: Ord a => SpatialSet a -> (a, a) -> Bool
contains Tvoid p = False
contains (Node p@(x,y) ne se sw nw) p'@(x',y') =
    if p == p' then True
    else if x < x' then
        if y < y' then
            contains ne p'
        else 
            contains se p'
    else
        if y < y' then
            contains nw p'
        else 
            contains sw p'


    -- Search element however the QuadTree doesn't satisfy it properties
    -- it could be possible after qmap
    -- This version is less eficient than 'contains'
contains_no_structured :: Ord a => SpatialSet a -> (a, a) -> Bool
contains_no_structured Tvoid p = False
contains_no_structured (Node p ne se sw nw) p'
    | p == p'   = True
    | otherwise = (contains ne p') || (contains se p') || (contains sw p') || (contains nw p')


    -- Question 6 --
    -- Search nearest point in SpatialSet from given point
nearest Tvoid p = p -- To implement


    -- Question 7 --
    -- Apply function for each element
qmap :: ((a, a) -> (b, b)) -> SpatialSet a -> SpatialSet b
qmap f Tvoid = Tvoid
qmap f (Node p Tvoid Tvoid Tvoid Tvoid) = plant (f p)
qmap f (Node p ne se sw nw) = (Node (f p) (qmap f ne) (qmap f se) (qmap f sw) (qmap f nw))
