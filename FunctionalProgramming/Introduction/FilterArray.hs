main = interact $ unlines . map show . solve . map read . words

solve :: [Int] -> [Int]
solve (n:xs) = filter' (<n) xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ []     = []
filter' p (x:xs) = if p x then x:filter' p xs else filter' p xs
