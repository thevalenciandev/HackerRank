main = interact $ show . solve . read . head . words

solve :: Integer -> Integer
solve n = product [1..n]
