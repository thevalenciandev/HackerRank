main = do _  <- getLine
          results <- solve . map read . words <$> getLine
          let arrays = map unwords results
          mapM_ putStrLn arrays

solve :: [Int] -> [[String]]
solve xs  = let (result,log) = solvehelper (last xs) (init xs) [] []
            in reverse log

solvehelper :: Int -> [Int] -> [Int] -> [[String]] -> ([Int], [[String]])
solvehelper x [] sortedxs sortinghistory = let result = x:sortedxs
                                           in (result, toLog result:sortinghistory)
solvehelper x unsortedxs sortedxs sortinghistory
    | l > x = let newunsorted = init unsortedxs
                  newsorted   = l:sortedxs
                  newhistory  = toLog (newunsorted ++ [l] ++ newsorted):sortinghistory
              in  solvehelper x newunsorted newsorted newhistory
    | otherwise = let result = unsortedxs ++ (x:sortedxs)
                  in  (result, toLog result:sortinghistory)
    where l = last unsortedxs

toLog :: [Int] -> [String]
toLog = map show
