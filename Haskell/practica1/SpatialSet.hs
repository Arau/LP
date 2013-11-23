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

cmp_spatial_set :: Ord a => SpatialSet a -> SpatialSet a -> Bool
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
    show tree = sTree tree (-1) ""

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
                                    sw_out = "\n" ++ (tab inc) ++ "<SO> "
                                    nw_out = "\n" ++ (tab inc) ++ "<NO> "

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
get_all :: SpatialSet a -> [(a,a)]
get_all Tvoid   = []
get_all (Node p ne se sw nw) = [p] ++ ( concat $ map (get_all) [ne,se,sw,nw] )


    -- Question 4 --
    -- Remove an element from SpatialSet
remove :: Ord a => SpatialSet a -> (a,a) -> SpatialSet a
remove Tvoid _ = Tvoid
remove (Node p@(x,y) ne se sw nw) p'@(x',y') = 
    if p == p' then
        build $ concat [(get_all ne), (get_all se), (get_all sw), (get_all nw)]
    else if x < x' then
        if y < y' then
            (Node p (remove ne p') se sw nw)
        else 
            (Node p ne (remove se p') sw nw)
    else
        if y < y' then
            (Node p ne se sw (remove nw p'))
        else
            (Node p ne se (remove sw p') nw)


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


    -- Question 6 --
    -- Search nearest point in SpatialSet from given point
nearest :: (Num a, Ord a) => SpatialSet a -> (a, a) -> (a, a)
nearest tree@(Node root _ _ _ _) p' = near_p tree p' root

near_p :: (Num a, Ord a) => SpatialSet a -> (a, a) -> (a, a) -> (a, a)
near_p Tvoid _ best_point = best_point;
near_p (Node p ne se sw nw) p' best_point
    | (exps p p') < (exps p' best_point) = min_dist p' (head l_point) l_point
    |  otherwise                         = min_dist p' (head l_bests) l_bests
    where
        l_point = [ near_p son p' p          | son <- [ne,se,sw,nw] ]
        l_bests = [ near_p son p' best_point | son <- [ne,se,sw,nw] ]


exps :: Num a => (a, a) -> (a, a) -> a
exps p@(x,y) p'@(x',y') = (x'- x)*(x'- x) + (y'- y)*(y'- y)


    -- Nearest point of list of points
min_dist :: (Num a, Ord a) => (a, a) -> (a, a) -> [(a, a)] -> (a, a)
min_dist p best_point [] = best_point
min_dist p best_point (x:xs)
    | (exps p x) < (exps p best_point)  = min_dist p x xs
    |  otherwise                         = min_dist p best_point xs


distance p@(x,y) p'@(x',y') = sqrt ((x'- x)**2 + (y'- y)**2)


-- Question 7 --
-- Apply function for each element
qmap :: ((a, a) -> (b, b)) -> SpatialSet a -> SpatialSet b
qmap f Tvoid = Tvoid
qmap f (Node p Tvoid Tvoid Tvoid Tvoid) = plant (f p)
qmap f (Node p ne se sw nw) = (Node (f p) (qmap f ne) (qmap f se) (qmap f sw) (qmap f nw))


    -- 7.2 apply translation for SpatialSet
translation :: Num a => a -> a -> SpatialSet a -> SpatialSet a
translation x y tree = qmap ( \(i,j) -> (x+i, y+j) ) tree


    -- 7.3 apply scale for SpatialSet
scale :: Num a => a -> SpatialSet a -> SpatialSet a
scale s tree = qmap ( \(i,j) -> (i*s, j*s) ) tree


    -- Question 8 --
    -- Accumulate all values with tail recursion (left side)
qfoldl :: (a -> (b, b) -> a) -> a -> SpatialSet b -> a
qfoldl f value  Tvoid = value
qfoldl f value (Node p ne se sw nw) = qfoldl f (qfoldl f (qfoldl f (qfoldl f (f value p) ne) se) sw) nw


    -- 8.2 --
    -- Get size of tree (number of nodes)
size :: Num n => SpatialSet a -> n
size Tvoid = 0
size tree  = qfoldl ( \x (a,b) -> x+1 ) 0 tree


    -- 8.3 --
    -- Bounding Box
    -- Implementation with qfold as question require,
    -- but less eficient (go over tree 4 times) than bound_box function.
box :: Ord a => SpatialSet a -> ((a,a), (a,a))
box tree@(Node p _ _ _ _) = ((left, top), (right, down))
    where
        left    = qfoldl( \l (x,y) ->
                            if  x < l then x
                            else l
                        ) (fst p) tree
        right   = qfoldl( \r (x,y) ->
                            if x > r then x
                            else r
                        ) (fst p) tree
        top     = qfoldl( \t (x,y) ->
                            if y > t then y
                            else t
                        ) (snd p) tree
        down    = qfoldl( \d (x,y) -> 
                            if y < d then y
                            else d
                        ) (snd p) tree




    -- Bounding box implementation with only one travel
    -- More complex than box function
bound_box :: Ord a => SpatialSet a -> ((a,a), (a,a))
bound_box tree@(Node p _ _ _ _) = limits (get_all tree) p p


    -- Get limits from list of points
    -- Result of 2 points:
    --      nw -> Left-top
    --      se -> Right-down
limits :: (Ord a) => [(a, a)] -> (a, a) -> (a, a) -> ((a, a), (a, a))
limits [] nw se = (nw, se)
limits (p:ps) nw@(xn,yn) se@(xs,ys) =
    if x < xn then
        if y < ys then
            limits ps (x, yn) (xs, y)
        else if y > yn then
            limits ps (x, y)  se
        else
            limits ps (x, yn) se
    else if x > xs then
        if y < ys then
            limits ps nw (x, y)
        else if y > yn then
            limits ps (xn, y) (x, ys)
        else
            limits ps nw (x, ys)
    else if y > yn then
            limits ps (xn, y) se
    else if y < ys then
            limits ps nw (xs, y)
    else
        limits ps nw se
    where
        x = fst p
        y = snd p

