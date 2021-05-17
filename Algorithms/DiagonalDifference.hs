-- n  : size of the chunks
-- xs : array to make into chunks
-- eg. chunkify 1 [1,2,3] => [[1],[2],[3]]
chunkify :: Int -> [a] -> [[a]]
chunkify 0 _  = []
chunkify n [] = []
chunkify n xs = take n xs : chunkify n (drop n xs)

-- n : Size of the matrix
-- m : Matrix of size n x n
-- Provides the diagonal of it as an array
diagonal :: Int -> [[a]] -> [a]
diagonal n m = [map (\r -> r !! i) m !! i | i <- [0..n-1]]
