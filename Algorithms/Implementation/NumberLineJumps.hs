main :: IO ()
main = interact $ solve . map read . words

solve :: [Int] -> String
solve [x1,v1,x2,v2]
  | v2 < v1 && (x1 - x2) `mod` (v1 - v2) == 0 = "YES"
  | otherwise = "NO"
