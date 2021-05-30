main = interact $ show . solve . read . head . words

solve :: Integer -> Integer
solve n = foldl (*) 1 [1..n]
