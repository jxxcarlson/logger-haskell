
{-# LANGUAGE ScopedTypeVariables #-}

module Exec (exec) where

import System.IO
import qualified Data.Text as T
import qualified Data.Text.IO as TI
import qualified Data.Vector as V ((!), forM_, map)
import Data.Csv
import qualified Data.String.HIUtils as DSU
import Control.Monad (mzero)
import qualified Data.ByteString.Lazy as BL
import Data.String.Utils (strip, split)
import GHC.Float

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
         "hours" -> hours datafile
         "total" -> totalHours datafile
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

hours :: FilePath -> IO ()
hours filePath = do
    content <- BL.readFile filePath
    case decode NoHeader content of
        Left err -> print err
        Right xs -> 
          let 
            hoursList =  V.forM_ xs $ \(date :: String, from :: String, to:: String, desc:: String) 
               -> print (DSU.padr 13 $ date ++ "   " ++  (DSU.padl 5 $ hm $ elapsed from to) ++ "    " ++ desc)

          in 
            hoursList

totalHours :: FilePath -> IO ()
totalHours filePath = do
    content <- BL.readFile filePath
    case decode NoHeader content of
        Left err -> print err
        Right xs -> 
          let 
            hours = V.map (\v -> elapsed (v V.! 1) (v V.! 2) ) xs
            totalHours = sum hours
          in 
            putStrLn (( hm $ Exec.roundTo 2 $ totalHours) ++ " => " ++ (show $ Exec.roundTo 2 $  totalHours * 110))


hm :: Double -> String
hm t = 
  let

    hours_ =  round t
    hours =  GHC.Float.int2Double (round t)
    minutes = round $ 60 * (t - hours)

    (hours__, minutes_) = if minutes < 0 then
        (hours_ - 1 ,minutes + 60)
      else
        (hours_, minutes)


  in (show hours__) ++ ":" ++ (show minutes_)
 
readHM :: [Char] -> (Double, Double)
readHM str =
  let 
    data_ = split ":" $ strip str
  in 
    (read (data_ !! 0), read (data_ !! 1))

elapsed :: String -> String -> Double
elapsed from to =
  let 
    ((hoursFrom, minutesFrom), (hoursTo, minutesTo)) = (readHM from, readHM to)
    elapsedHours = hoursTo - hoursFrom
    elapsedMinutes = minutesTo - minutesFrom
  in
    elapsedHours + (elapsedMinutes / 60.0)

roundTo n f =  (fromInteger $ round $ f * (10^n)) / (10.0^^n)