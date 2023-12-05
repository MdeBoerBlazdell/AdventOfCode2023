import Data.Text (splitOn, pack, unpack)
import Data.Set (fromList, intersection)

type Card = ([Int], [Int])

parseCard :: String -> Card
parseCard s = (ownNr, winningNr)
    where splitted = splitOn (pack ":") (pack s)
          cardsSplitted = splitOn (pack " | ") (last splitted)
          ownNr = map read $ words $ unpack $ head cardsSplitted
          winningNr = map read $ words $ unpack $ last cardsSplitted

matches :: Card-> Int
matches (xs, ys) = length $ fromList xs `intersection` fromList ys

score :: Int -> Int
score 0 = 0
score 1 = 1
score n = 2 * score (n - 1)

winCards :: [Int] -> [Int]
winCards [] = []
winCards (x : xs) = 1+ sum (take x (winCards xs)) : winCards xs

part1:: [Card] -> Int
part1 xs = sum (map (score . matches) xs)

part2 :: [Card] -> Int
part2 xs = sum $ winCards $ map matches xs 

main :: IO ()
main = do
    input <- readFile "inputs/day4.txt"
    let cards = map parseCard $ lines input
    print $ part1 cards
    print $ part2 cards
