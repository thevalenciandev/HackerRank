main = interact $ unlines . map show . f . map read . words

f :: [Int] -> [Int]
f xs = map snd . filter (\(i,_) -> even i) $ [(i,x) | (i,x) <- zip [1..length xs] xs]
