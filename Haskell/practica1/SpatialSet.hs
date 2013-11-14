module SpatialSet where

    -- Question 1 --
    -- SpatialSet defining a QuadTree
    -- @QuadTree children -> NE, SE, SW, NW
data SpatialSet a  = Node (a,a) (SpatialSet a) (SpatialSet a) (SpatialSet a) (SpatialSet a)
                   | Tvoid
                   deriving (Show)


    -- Comparision between 2 SpatialSet
instance (Eq a, Num a) => Eq (SpatialSet a) where
    qTree1 == qTree2    = cmp_quad_tree qTree1 qTree2

cmp_quad_tree :: (Eq a, Num a) => SpatialSet a -> SpatialSet a -> Bool
cmp_quad_tree (Node _ _ _ _ _)  Tvoid           = False
cmp_quad_tree Tvoid            (Node _ _ _ _ _) = False
cmp_quad_tree Tvoid             Tvoid           = True
cmp_quad_tree t1@(Node p1 ne1 se1 sw1 nw1) t2@(Node p2 ne2 se2 sw2 nw2)
    | l1 /= l2  = False
    | otherwise = True
    where
        l1 = get_all t1
        l2 = get_all t2


    -- Question 3 --
    -- Get a list of all elements of SpatialSet
get_all :: (Num a) => SpatialSet a -> [(a,a)]
get_all Tvoid   = []
get_all (Node p ne se sw nw) = [p] ++ ( concat $ map (get_all) [ne,se,sw,nw] )
