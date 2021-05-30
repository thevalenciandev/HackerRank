main = interact $ show . solve . map read . words

solve :: [Int] -> Int
solve (_:k:cs) = 100 - unitse
    where jumps  = takeWhile (/=0) [i `mod` length cs | i <- [k,k+k..]] ++ [0]
          clouds = [cs !! i | i <- jumps]
          unitse = foldl (-) 0 $ map (\x -> if x == 0 then -1 else -3) clouds
