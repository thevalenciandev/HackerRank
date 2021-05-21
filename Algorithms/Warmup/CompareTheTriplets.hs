main :: IO ()
main = do xs <- getLine
          ys <- getLine
          putStrLn $ unwords $ map show $ solve (toIntArray xs) (toIntArray ys)

toIntArray :: String -> [Int]
toIntArray = map read . words

solve :: [Int] -> [Int] -> [Int]
solve xs ys = [alices, bobs]
    where scores = [x - y | (x,y) <- zip xs ys]
          alices = length $ filter (>0) scores
          bobs   = length $ filter (<0) scores
