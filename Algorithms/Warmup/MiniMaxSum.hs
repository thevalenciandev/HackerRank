import Data.List

main :: IO ()
main = interact $ unwords . map show . solve . map read . words

solve :: [Int] -> [Int]
solve xs = [minsum, maxsum]
    where sorted = sort xs
          minsum = sum $ init sorted
          maxsum = sum $ tail sorted
