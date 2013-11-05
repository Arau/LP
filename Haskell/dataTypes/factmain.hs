import System.Environment (getArgs)

factorial 0 = 1
factorial n = n * factorial (n-1)

main = do
          [input] <- getArgs
	  print (factorial (read input)) 


