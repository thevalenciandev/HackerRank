import Data.List

main :: IO ()
main = interact $ show . solve . map read . words . last . lines

solve :: [Int] -> Int
solve = length . last . group . sort
