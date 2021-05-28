main = do t <- read . head . words <$> getLine
          results <- sequence (replicate t (show . solve . map read . words <$> getLine))
          mapM_ putStrLn results

-- n : number of prisoners 1<=n<=10e9
-- m : number of sweets    1<=m<=10e9
-- s : chair number to start 1<=s<=n
solve :: [Int] -> Int
solve [n,m,s] = (m-1+s-1) `mod` n + 1
