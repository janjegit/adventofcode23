module Main where

import Day01
import Day02

runDay :: (String,(String -> Int)) -> IO()
runDay (path, f)= do 
  udata <- readFile path
  let result = f udata
  print result

d01 :: (String, (String -> Int))
d01 = ("data/d01part1.txt", calibration . getCalibration)
d02 :: (String, (String -> Int))
d02 = ("data/d02part1.txt", sumValidGames) 


main :: IO ()
main = putStrLn "Hello, Haskell!"
