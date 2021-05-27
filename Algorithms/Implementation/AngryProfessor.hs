main = do [n] <- getList
          results <- sequence (replicate n (solve . concat <$> getScenario))
          mapM_ putStrLn results

getList :: IO [Int]
getList = map read . words <$> getLine

getScenario :: IO [[Int]]
getScenario = sequence (replicate 2 getList)

-- Returns NO if the class goes ahead and YES if cancelled
solve :: [Int] -> String
solve (_:k:a) = if ontimestudents >= k then "NO" else "YES"
    where ontimestudents = length $ filter (<=0) a
