main = interact $ unlines . solve . read . head . lines

solve :: Int -> [String]
solve n = take n [replicate (n-i) ' ' ++ replicate i '#' | i <- [1..]]
