main :: IO ()
main = interact $ show . solve . map read . words

solve :: [Int] -> Int
solve [n,p] = min fromStart fromEnd
    where fromStart = p `div` 2
          fromEnd   = n `div` 2 - fromStart
