main :: IO ()
main = do [b,_,_]   <- getList
          keyboards <- getList
          drives    <- getList
          putStrLn $ show $ solve b keyboards drives

getList :: Read a => IO [a]
getList = map read . words <$> getLine

solve :: Int -> [Int] -> [Int] -> Int
solve b keyboards drives =
    -- Adding a -1 value to cater for the case when everything is above
    -- budget, which would return an empty list and fail the maximum fn at runtime
    maximum $ filter (<=b) $ (-1) : [k+d | k<-keyboards, d<-drives]
