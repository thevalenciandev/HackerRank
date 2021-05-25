import Data.Function
import Data.List

main :: IO ()
main = interact $ show . solve . map read . tail . words

solve :: [Int] -> Int
solve = length .
        maximumBy (compare `on` length) .
        groupBy (\x y -> abs(x-y)<=1) .
        sort -- No constraint on the actual order of elements in subarray
