main :: IO ()
main = interact $ unlines . map show . solve . map read . tail . lines

solve :: [Int] -> [Int]
solve xs = map grade xs

grade :: Int -> Int
grade x | x < 38 || (nmof - x) > 2 = x
        | otherwise                = nmof
        where nmof = nextmult5 x

nextmult5 :: Int -> Int
nextmult5 x = case x `mod` 5 of
                0 -> x
                otherwise -> nextmult5 (x + 1)
