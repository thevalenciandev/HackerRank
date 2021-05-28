main = interact $ show . solve . read . head . words

-- This will overflow at solve 111 but constraints are
-- 1 <= n <= 50 so we can use Int
solve :: Int -> Int
solve n = foldl1 (+) $ 
          take n $ 
          iterate (\x -> x * 3 `div` 2) 2
