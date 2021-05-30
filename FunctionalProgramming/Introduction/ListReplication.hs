main = interact $ unlines . map show . solve . map read . words

solve :: [Int] -> [Int]
solve (s:xs) = concat . map (replicate s) $ xs
