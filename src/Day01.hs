module Day01 where

import Data.Char (isDigit, digitToInt)

testInput :: String
testInput = "1abc2\npqr3stu8vwx\na1b2c3d4e5f\ntreb7uchet"

calibration :: [Int] -> Int
calibration xs = sum xs

getCalibration :: String -> [Int]
getCalibration = 
  map ((\xs -> 10 * digitToInt (head xs) + digitToInt (last xs)) . filter
  isDigit) . words
