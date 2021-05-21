import Data.List

main :: IO ()
main = interact $ unwords . map show . solve . map read . words . last . lines

solve :: [Int] -> [Int]
solve xs = [best, worst]
    where best  = (length $ group $ map maximum $ tail $ inits xs) - 1
          worst = (length $ group $ map minimum $ tail $ inits xs) - 1
