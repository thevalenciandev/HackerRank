main :: IO ()
main = do [_,k] <- getList
          xs    <- getList
          [b]   <- getList
          let tr = totalrefund k b xs
          if tr == 0 then
            putStrLn "Bon Appetit"
          else
            putStrLn $ show tr
          
getList :: Read a => IO [a]
getList = do l <- getLine
             return $ map read $ words l

totalrefund :: Int -> Int -> [Int] -> Int
totalrefund k b xs = b - (billsplit k xs)

billsplit :: Int -> [Int] -> Int
billsplit k xs = ((sum $ fst spl) + (sum $ tail $ snd spl)) `div` 2
    where spl = splitAt k xs
