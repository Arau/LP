module Game where

import System.Random

data Slot = Black | Red | Empty
  deriving (Eq, Show)

data Line  = Line Slot Slot Slot
  deriving (Show)
data State = Board Line Line Line
  deriving (Show)
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
winner board@(Board l1 l2 l3)
    | wl1  /= (-1) = wl1
    | wl2  /= (-1) = wl2
    | wl3  /= (-1) = wl3
    | wc   /= (-1) = wc
    | wd   /= (-1) = wd
    | otherwise    = -1
    where 
        wl1 = winnerLine l1
        wl2 = winnerLine l2
        wl3 = winnerLine l3
        wc =  winnerCol board
        wd  = winnerDiagonal board


winnerLine :: Line -> Int
winnerLine l@(Line s1 s2 s3) =
    if (eqList l)    then
        if (s1 == Black)    then 0
        else if (s1 == Red) then 1
        else (-1)
    else (-1)


eqList :: Line -> Bool
eqList (Line s1 s2 s3) | (s1 == s2 && s2 == s3) = True
eqList _ = False


winnerCol :: State -> Int
winnerCol (Board (Line s11  _ _) (Line s21 _ _) (Line s31 _ _)) | (s11 == s21 && s21 == s31) && s11 == Black = 0
winnerCol (Board (Line s11  _ _) (Line s21 _ _) (Line s31 _ _)) | (s11 == s21 && s21 == s31) && s11 == Red   = 1
winnerCol (Board (Line _  s12 _) (Line _ s22 _) (Line _ s32 _)) | (s12 == s22 && s22 == s32) && s12 == Black = 0
winnerCol (Board (Line _  s12 _) (Line _ s22 _) (Line _ s32 _)) | (s12 == s22 && s22 == s32) && s12 == Red   = 1
winnerCol (Board (Line _ _  s13) (Line _ _ s32) (Line _ _ s33)) | (s13 == s32 && s32 == s33) && s13 == Black = 0
winnerCol (Board (Line _ _  s13) (Line _ _ s32) (Line _ _ s33)) | (s13 == s32 && s32 == s33) && s13 == Red   = 1
winnerCol _ = (-1)


winnerDiagonal :: State -> Int
winnerDiagonal (Board (Line s11  _ _) (Line  _ s22 _) (Line _ _ s33)) | (s11 == s22 && s22 == s33) && s11 == Black = 0
winnerDiagonal (Board (Line s11  _ _) (Line  _ s22 _) (Line _ _ s33)) | (s11 == s22 && s22 == s33) && s11 == Red   = 1
winnerDiagonal (Board (Line  _ _ s13) (Line  _ s22 _) (Line s31 _ _)) | (s13 == s22 && s22 == s31) && s22 == Black = 0
winnerDiagonal (Board (Line  _ _ s13) (Line  _ s22 _) (Line s31 _ _)) | (s13 == s22 && s22 == s31) && s22 == Red   = 1
winnerDiagonal _ = (-1)

strategyNull :: Strategy
strategyNull s g = ((1,1), g)

