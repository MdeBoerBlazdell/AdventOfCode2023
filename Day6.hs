import Data.Text (splitOn, pack, unpack, split)
import Data.Char (isDigit)

parseRace1 :: String -> String -> [(Int, Int )]
parseRace1 ts ds = times `zip` distances 
    where times = map read $ words ts 
          distances = map read $ words ds

parseRace2 :: String -> String -> (Int, Int)
parseRace2 ts ds = (time,distance)
    where time = read $ filter isDigit ts :: Int 
          distance = read $ filter isDigit ds :: Int 

waysToWin :: (Int, Int) -> Int
waysToWin (t, d) = length $ [x | x <- [1 .. t], x * (t - x) > d]


waysToBeat :: Int -> Int -> (Int, Int)
waysToBeat time distance = (ceiling $ 0.5 * (fromIntegral time - (sqrt . fromIntegral) (time * time - 4 * (distance + 1))), 
                            ceiling $ 0.5 * (fromIntegral time + (sqrt . fromIntegral) (time * time - 4 * (distance + 1))))

part1:: String -> String  -> Int
part1 ts ds = product $ map waysToWin $ parseRace1 ts ds

part2:: String -> String -> Int
part2 ts ds = (\(a,b) -> abs(a-b)) . uncurry waysToBeat $ parseRace2 ts ds

main :: IO ()
main = do
    input <- readFile "inputs/day6.txt"
    let splitInput =  lines $ filter (\x -> isDigit x || x == '\n' || x == ' ') input 
    print $ part1 (head splitInput) (last splitInput)
    print $ part2 (head splitInput) (last splitInput)