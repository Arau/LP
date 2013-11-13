module SpatialSet where

    -- Question 1 --
    -- SpatialSet defining a QuadTree
    -- @QuadTree children -> NE, SE, SO, NO
data SpatialSet a  = Node a  (SpatialSet a) (SpatialSet a) (SpatialSet a) (SpatialSet a)
                   | Leaf a
                   deriving (Eq, Show)


data Set = Pairs [ (Latitude, Longitude) ]
 deriving (Eq, Show)


    -- Alias to define a point in space --
type Latitude  = Float
type Longitude = Float


