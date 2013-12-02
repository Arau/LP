import Game
import System.Random

main :: IO ()
main = do putStrLn "Game start"
          randomGenerator <- getStdGen
          firstPhase randomGenerator 0 emptyBoard

firstPhase :: StdGen -> Int -> State -> IO()
firstPhase randomGenerator turn state = do readPlayerMovement randomGenerator turn state

readPlayerMovement :: StdGen -> Int -> State -> IO()
readPlayerMovement randomGenerator turn state = do
    putStrLn "Write your movement:"
    playerMovement <- getLine
    if (valid (read playerMovement) Red Red state) then
       performPlayerMovement randomGenerator turn state (read playerMovement)
    else
       readPlayerMovement randomGenerator turn state

    -- Execute player movement and pass
    -- modified board to computer player
performPlayerMovement :: StdGen -> Int -> State -> Movement -> IO()
performPlayerMovement randomGenerator turn state movement = performComputerMovement randomGenerator turn modifiedState
    where
        modifiedState = (move movement Red state)

performComputerMovement :: StdGen -> Int -> State -> IO()
performComputerMovement randomGenerator turn state = do
    putStrLn ("Computer Movement: " ++ (show computerMovement))
    if ((winner state == 1))         then putStrLn "Player wins."
    else if ((winner newState) == 2) then putStrLn "Computer wins."
    else if (turn < 3) then
       firstPhase  randomGenerator2 (turn+1) newState
    else
       secondPhase randomGenerator2 (turn+1) newState

    where
        (computerMovement, randomGenerator2) = (strategyNull state randomGenerator)
        newState = (move computerMovement Black state)


    -- Perform 2 moves to change a token between boxes
    -- First to put blank and second to put token in other
    -- empty Slot
secondPhase :: StdGen -> Int -> State -> IO()
secondPhase randomGenerator turn state = do
    putStrLn "Remove one of your tokens:"
    playerMovement <- getLine
    if (valid (read playerMovement) Empty Red state) then
        performRemoveOneToken randomGenerator turn state (read playerMovement)
    else
        do 
            putStrLn "Invalid movement"
            secondPhase randomGenerator turn state

performRemoveOneToken :: StdGen -> Int -> State -> Movement -> IO()
performRemoveOneToken randomGenerator turn state movement = readPlayerMovement randomGenerator turn modifiedState
  where 
    modifiedState = (move movement Empty state)
