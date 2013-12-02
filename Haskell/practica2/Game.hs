module Game where

import System.Random


data Slot = Black | Red | Empty
  deriving (Eq, Show)

data Line  = Line Slot Slot Slot
data State = Board Line Line Line

type Movement = (Int, Int)
type Strategy = State -> StdGen -> (Movement, StdGen)


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
valid (x,_) _ _ _ | (x < 1 || x > 3) = False
valid (_,y) _ _ _ | (y < 1 || y > 3) = False
valid (1,y) s s1 (Board l1 _  _) | (is_eq y s l1) = False | not (is_empty y l1) = False
valid (2,y) s (Board _ l2  _) | (is_eq y s l2) = False | not (is_empty y l2) = False
valid (3,y) s (Board _ _  l3) | (is_eq y s l3) = False | not (is_empty y l3) = False
valid _ _ _ = True


  -- Check if slot is the same as player wants to put in
is_eq :: Int -> Slot -> Line -> Bool
is_eq 1 s (Line s' _ _) | s == s' = True
is_eq 2 s (Line _ s' _) | s == s' = True
is_eq 3 s (Line _ _ s') | s == s' = True
is_eq y s Line = False


    -- Check if slot is empty
is_empty :: Int -> Line -> Bool
is_empty 1 (Line Empty  _      _    ) = True
is_empty 2 (Line _      Empty  _    ) = True
is_empty 3 (Line _      _      Empty) = True
is_empty y  line = False
