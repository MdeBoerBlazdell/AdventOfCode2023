module Day2 where

import Data.Char (digitToInt, isDigit)
import Data.Text (splitOn, pack, unpack)
import Data.List (groupBy, sortOn)

type Game  = (Int, [(Int, Colour)])
data Colour = R | G | B
    deriving (Show, Ord, Eq) 

toGame :: String -> Game
toGame s = (gameNr, concatMap parsePulls pulls)
    where parsedString = splitOn (pack ":") (pack s)
          gameNr = read $ last $ words $ unpack $ head parsedString
          pulls = map unpack $ splitOn (pack ";") (last parsedString)

parsePulls :: String -> [(Int, Colour)]
parsePulls xs = map toPair pairs
    where pairs = map unpack $ splitOn (pack ",") (pack xs)

toPair :: String -> (Int, Colour)
toPair s = (amount, c) 
    where pair = words s
          amount = read (head pair) :: Int
          c = case last pair of
            "green" -> G
            "red" -> R
            "blue" -> B

validPull :: (Int, Colour) -> Bool
validPull (count,colour)
    | count > 12 && colour == R = False
    | count > 13 && colour == G = False
    | count > 14 && colour == B = False
    | otherwise = True

validGame :: Game -> Bool
validGame (_, pulls) = length pulls == length (filter validPull pulls) 

toPower :: Game -> Int
toPower (_, xs) =   product powers 
    where powers = map (maximum . map fst) $ groupBy(\x y -> snd x == snd y) $ sortOn snd xs  

part1:: [String] -> Int
part1 xs = sum . map fst $ filter validGame $ map toGame xs

part2:: [String] -> Int
part2 = sum . map (toPower . toGame)

main :: IO ()
main = do
    input <- readFile "inputs/day2.txt"
    print $ part1 $ lines input
    print $ part2 $ lines input