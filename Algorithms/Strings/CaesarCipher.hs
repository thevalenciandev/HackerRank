import Data.Char

main = do _  <- getLine
          xs <- getLine
          k  <- readLn
          putStrLn $ solve xs k

solve :: String -> Int -> String
solve xs n = map (\x -> transform x n) xs

transform x n = if isLower x then
                    encrypt x n 'a'
                else if isUpper x then
                      encrypt x n 'A'
                else x

-- Assumes input is only from alphabet
index c s = ord c - ord s

-- The index result is 0-based and rotates back
-- to the front of the alphabet
-- eg. 0 = 'a' or 0 = 'A', 1 = 'b' or 1 = 'B', etc
applyshift c n s = (index c s + n) `mod` 26

encrypt c n s = chr (applyshift c n s + ord s)
