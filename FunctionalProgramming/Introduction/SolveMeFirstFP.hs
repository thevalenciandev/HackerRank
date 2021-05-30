main = interact $ show . solve . map read . words

solve :: [Int] -> Int
solve [a,b] = a + b
