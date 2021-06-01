main = do [_,k,q] <- getNumbers 
          xs      <- getNumbers
          qs      <- sequence $ replicate q readLn
          mapM_ putStrLn $ map show $ solve xs k qs

getNumbers :: Read a => IO [a]
getNumbers = map read . words <$> getLine

solve :: [Int] -> Int -> [Int] -> [Int]
solve xs k qs = map (\q -> xs !! (index q)) qs
    where len     = length xs
          shift   = k `mod` len
          index q = if (q-shift) < 0 then len + (q-shift) else (q-shift)
