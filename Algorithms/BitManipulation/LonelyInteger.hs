import Data.Bits

main = interact $ show . solve . map read . tail . words

solve :: [Int] -> Int
solve = foldl1 xor
