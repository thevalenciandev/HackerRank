main :: IO ()
main = do nk <- getLine
          ar <- getLine
          putStrLn $ show $ solve (read $ last $ words nk) (map read $ words ar)

solve :: Int -> [Int] -> Int
solve k ar = length $ divsumpairs k ar 

divsumpairs :: Int -> [Int] -> [(Int,Int)]
divsumpairs k ar = filter (\(i,j) -> (i+j) `mod` k == 0) $ 
                   pairs ar

-- Pairs such that i<j
pairs :: [a] -> [(a,a)]
pairs xs | length xs < 2 = []
         | otherwise     = [(head xs,y) | y<-tail xs] ++ pairs (tail xs)
