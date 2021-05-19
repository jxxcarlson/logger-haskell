
{-# LANGUAGE ScopedTypeVariables #-}

module Exec (exec) where

import System.IO
import qualified Data.Text as T
import qualified Data.Text.IO as TI
import qualified Data.Vector as V
import Data.Csv
import Control.Monad (mzero)
import qualified Data.ByteString.Lazy as BL

datafile = "log.csv"

exec :: String -> IO ()
exec str = 
  case words str of
     [] -> putStrLn "??"
     (cmd:args) -> 
       case cmd of
         "help" -> help
         "show" -> showData
         "log" -> logItem
         "read" -> readCSV datafile
         _ -> putStrLn "??"

help = putStrLn "Commands: quit, show, log"


showData = do
  contents <- TI.readFile datafile
  TI.putStrLn contents
 

logItem = do
    input <- getLine
    contents <- TI.readFile datafile
    let newContents = T.append contents (T.pack ("\n" ++ input))
    TI.writeFile datafile newContents
    putStrLn $ "data written to " ++ datafile



readCSV :: FilePath -> IO ()
readCSV filePath = do
    content <- BL.readFile filePath
    case decode NoHeader content of
        Left err -> print err
        Right xs -> V.forM_ xs $ \(date :: String, from :: String, to:: String, desc:: String) -> print (date, from, to, desc)
