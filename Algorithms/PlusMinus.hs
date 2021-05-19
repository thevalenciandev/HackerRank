main :: IO ()
main = interact $ unlines . map show . solve . map read . words . last . lines

solve :: [Int] -> [Float]
solve xs = undefined
