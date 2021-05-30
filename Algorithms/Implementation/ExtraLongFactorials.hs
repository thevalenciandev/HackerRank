main = interact $ show . solve . read . head . words

solve :: Integer -> Integer
solve 1 = 1
solve n = n * solve (n - 1)
