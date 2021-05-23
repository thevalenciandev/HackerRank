import Data.List

main :: IO ()
main = interact $ show . solve . last . words

solve :: String -> Int
solve = length . 
        filter (all (<0)) . 
        groupBy (\x y -> x/=0 && y/=0) . 
        scanl (+) 0 . 
        map delta

delta :: Char -> Int
delta 'U' = 1
delta 'D' = -1
