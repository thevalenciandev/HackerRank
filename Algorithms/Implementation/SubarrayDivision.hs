main :: IO ()
main = do getLine -- ignore this line
          xs <- getLine
          md <- getLine
          putStrLn $ show $ solve (map read $ words xs) 
                                  (read $ head $ words md) 
                                  (read $ last $ words md) 

solve :: [Int] -> Int -> Int -> Int
solve xs d m = length $ filter (==d) $ map sum $ window m xs

window :: Int -> [a] -> [[a]]
window x xs | length xs >= x = take x xs : window x (tail xs)
            | otherwise      = []
