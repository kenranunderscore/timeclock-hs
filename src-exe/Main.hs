module Main where

import qualified Timeclock

main :: IO ()
main = do
  putStrLn "Enter path to timelog: "
  path <- getLine
  putStrLn $ "Parsing timelog file '" ++ path ++"'..."
  print =<< Timeclock.readTimelog path
