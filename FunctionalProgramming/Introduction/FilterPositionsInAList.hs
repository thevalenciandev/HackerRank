main = interact $ unlines . map show . f . map read . words

f :: [Int] -> [Int]
f xs = map snd . filter (even . fst) . zip [1..] $ xs
