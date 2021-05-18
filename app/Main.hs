import           System.IO  
import           Control.Monad (unless)
import           System.IO
import           Exec (exec)



main :: IO()
main =
  do
    putStrLn "\nWelcome to Lox"
    repl


repl :: IO()
repl =
  do
    putStrLn "\nHello!  Type quit to quit, help for help\n"
    loop

loop :: IO ()
loop = do
  putStr " > " >> hFlush stdout 
  input <- getLine 
  unless (input == "quit")
       $ exec input 
      >> loop


