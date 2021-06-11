import Data.List

main = interact $ unlines . map show . solve . map read . tail . words

solve :: [Integer] -> [Integer]
solve = sort
