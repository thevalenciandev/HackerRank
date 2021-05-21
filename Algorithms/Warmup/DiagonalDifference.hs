import Data.List

main = do n <- getLine
          m <- sequence (replicate (read n) getLine)
          putStrLn $ show $ solve (read n) (toMatrix n m)

toMatrix :: String -> [String] -> [[Int]]
toMatrix n xs = chunkify (read n) (map read $ concat $ map words xs)

solve :: Int -> [[Int]] -> Int
solve n m = abs (sum ltr - sum rtl)
  where ltr = diagonal n m
        rtl = diagonal n (map reverse m)

-- n  : size of the chunks
-- xs : array to make into chunks
-- eg. chunkify 1 [1,2,3] => [[1],[2],[3]]
chunkify :: Int -> [a] -> [[a]]
chunkify 0 _  = []
chunkify n [] = []
chunkify n xs = take n xs : chunkify n (drop n xs)

-- n : Size of the matrix
-- m : Matrix of size n x n
-- Provides the left-to-right diagonal of it as an array
diagonal :: Int -> [[a]] -> [a]
diagonal n m = [m !! i !! i | i <- [0..n-1]]
