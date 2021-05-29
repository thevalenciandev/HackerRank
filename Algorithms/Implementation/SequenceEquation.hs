import Data.Maybe
import Data.List

main = interact $ unlines . map show . solve . map read . words

-- 1. Find where x is and take its index
-- 2. Now find the number in the array which is = to that index and take the index of it
solve :: [Int] -> [Int]
solve (n:ps) = [calcppy x ps | x <- [1..n]]
    where calcpx x ps  = fromJust $ (+1) <$> elemIndex x ps
          calcppy x ps = fromJust $ (+1) <$> elemIndex (calcpx x ps) ps
