module Game where

import System.Random

data Slot = Black | Red | Empty
  deriving (Eq, Show)

data Line  = Line Slot Slot Slot
data State = Board Line Line Line

type Movement = (Int, Int)
type Strategy = State -> StdGen -> (Movement, StdGen)

emptyBoard:: State
emptyBoard = Board (Line Empty Empty Empty) (Line Empty Empty Empty) (Line Empty Empty Empty)

move :: Movement -> Slot -> State -> State
move (1,y) s (Board l1 l2 l3) = (Board (inline y s l1) l2 l3)
move (2,y) s (Board l1 l2 l3) = (Board l1 (inline y s l2) l3)
move (3,y) s (Board l1 l2 l3) = (Board l1 l2 (inline y s l3))


inline :: Int -> Slot -> Line -> Line
inline 1 s (Line _ s2 s3) = (Line s  s2 s3)
inline 2 s (Line s1 _ s3) = (Line s1 s  s3)
inline 3 s (Line s1 s2 _) = (Line s1 s2 s)


    -- Check if movement can be done
    -- Movement: position where to change the State
    -- Slot: item to put in the position Movement
    -- Slot: identifier of the player performing the action
    -- State: current state of the game
valid :: Movement -> Slot -> Slot -> State -> Bool
valid _ _ _ _ = True

winner :: State -> Int
winner _ = 1

strategyNull :: Strategy
strategyNull s g = ((1,1), g)

