main :: IO ()
main = interact $ unlines . map show . solve . map read . words . last . lines

solve :: [Int] -> [Float]
solve xs = [ratpos,ratneg,ratzer]
    where ratpos = ratio (>0)  xs
          ratneg = ratio (<0)  xs
          ratzer = ratio (==0) xs

ratio :: (Int -> Bool) -> [Int] -> Float
ratio p xs = fromIntegral (length(filter (p)  xs)) / fromIntegral (length xs)
