main = interact $ show . solve . map read . words

solve :: [Int] -> Int
solve [i,j,k] = length $ filter beautifulnum $ map calculatenum [i..j]
    where beautifulnum n = n `mod` k == 0
          calculatenum d = abs $ d - reversenum d
          reversenum = read . reverse . show
