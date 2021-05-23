import Data.Function
import Data.List

main :: IO ()
main = interact $ show . solve . map read . tail . words

solve :: [Int] -> Int
solve = head . head . sortBy (flip compare `on` length) . group . sort
