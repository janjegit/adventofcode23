module Day02 where

import Data.Char 
import Data.Maybe
import Data.List.Split

data Color = Red Int | Green Int | Blue Int
  deriving (Show, Eq)

type Bag    = (Int,Int,Int)
type GameId = Int
type Game   = (GameId ,[Color])

bag :: Bag
bag = (12, 13, 14)

parseGame :: String -> Maybe Game
parseGame [] = Nothing
parseGame xs = Just (gameId, map (\x -> (getColor x) (getNumber x)) cleanLine)
  where 
    cleanLine = 
      concatMap (splitOn ";") $ splitOn "," $ filter ((/=) ' ') $ (drop
      (parseFrom xs 0) xs)
    
    getNumber s = go 0 (digitList s)

    digitList s = reverse $ map digitToInt $ filter isDigit s
    go _ [] = 0
    go p (y:ys) = (y*10^p) + (go (p+1) ys)

    getColor s = color $ filter isAlpha s
      where
        color c = case c of
          "red" -> Red
          "green" -> Green 
          "blue" -> Blue 
          _ -> error "Unknown color string."

    gameId = go 0 $ digitList $ take (parseFrom xs 0) xs
    parseFrom [] _ = 0
    parseFrom (z:zs) count | z == ':' = count
                           | otherwise = parseFrom zs (count+1)

sumValidGames :: String -> Int
sumValidGames xs = add validGames 0
  where
    validGames = 
      filter (\x -> isValidGame $ snd $ fromJust x) (map parseGame $ lines xs)
    add [] acc = acc
    add (y:ys) acc = add ys (acc + (fst $ fromJust y))
 

isValidGame :: [Color] -> Bool
isValidGame []     = True
isValidGame (x:xs) = 
  let (maxR,maxG,maxB) = bag  
  in case x of 
    Red   r -> isValid r maxR
    Green g -> isValid g maxG
    Blue  b -> isValid b maxB
  where
    isValid c cs | c <= cs = isValidGame xs
                 | otherwise = False
