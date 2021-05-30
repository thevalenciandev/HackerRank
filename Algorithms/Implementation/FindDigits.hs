import Data.Char

main = do t <- getNumber 
          results <- sequence (replicate t (show . solve <$> getNumber))
          mapM_ putStrLn results

getNumber :: Read a => IO a
getNumber = read . head . words <$> getLine

solve :: Int -> Int
solve n = foldl (\acc x -> if isDivisor (digitToInt x) n then acc+1 else acc) 0 $ show n
    where isDivisor x n = x /= 0 && n `mod` x == 0
