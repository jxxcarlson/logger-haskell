module Exec (exec) where

exec :: String -> IO ()
exec str = 
  case words str of
     [] -> putStrLn "??"
     (cmd:args) -> 
       case cmd of
         "help" -> help
         -- "read" -> read args
         _ -> putStrLn "??"

help = putStrLn "No help available at this time"