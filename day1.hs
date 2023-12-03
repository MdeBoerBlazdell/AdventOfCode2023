import Data.Char (isDigit, digitToInt, isNumber)

part1 :: [String] -> Int
part1 = sum . map val
    where val s = 10 * head (digits s) + last (digits s)
          digits s = map digitToInt $ filter isDigit s

replaceWith :: String -> [Int]
replaceWith [] = []
replaceWith ('o':'n':'e':xs) = 1 : replaceWith ('e':xs)
replaceWith ('t':'w':'o':xs) = 2 : replaceWith ('o':xs)
replaceWith ('t':'h':'r':'e':'e':xs) = 3 : replaceWith ('e':xs)
replaceWith ('f':'o':'u':'r':xs) = 4 : replaceWith xs
replaceWith ('f':'i':'v':'e':xs) = 5 : replaceWith ('e':xs)
replaceWith ('s':'i':'x':xs) = 6 : replaceWith xs
replaceWith ('s':'e':'v':'e':'n':xs) = 7 : replaceWith ('n':xs)
replaceWith ('e':'i':'g':'h':'t':xs) = 8 : replaceWith ('t':xs)
replaceWith ('n':'i':'n':'e':xs) = 9 : replaceWith ('e':xs)
replaceWith (x:xs)
    | isNumber x = digitToInt x : replaceWith xs
    | otherwise = replaceWith xs

part2:: [String] -> Int
part2 = sum . map ((\xs -> 10 * head xs + last xs) . replaceWith)

solve :: IO ()
solve = do
    input <- readFile "inputs/day1.txt"
    print $ part1 $ lines input
    print $ part2 $ lines input