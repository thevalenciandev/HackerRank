import Data.List

main = interact $ show . solve . map read . tail . words

solve :: [Int] -> Int
solve = head . head . dropWhile (\xs -> 1 /= length xs) . group . sort
