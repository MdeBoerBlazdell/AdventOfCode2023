import Data.Char (ord)
import Data.Text (splitOn, pack, unpack)

parseInput :: String -> [String]
parseInput s = map unpack (splitOn (pack ",") (pack s))

hash :: String -> Int
hash = foldl (\acc x -> (acc + ord x) * 17 `rem` 256) 0

part1 :: [String] -> Int
part1 xs = sum $ map hash (concatMap parseInput xs)

main :: IO ()
main = do
    input <- readFile "inputs/day15.txt"
    print $ part1 (lines input)