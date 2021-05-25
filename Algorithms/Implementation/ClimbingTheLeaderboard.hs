import Data.List

main = do getLine -- ignore n
          ranked <- getList
          getLine -- ignore m
          player <- getList
          putStrLn $ unlines $ map show $ solve ranked player

getList :: Read a => IO [a]
getList = map read . words <$> getLine

solve :: [Int] -> [Int] -> [Int]
solve board scores = [calcrank s | s <- scores]
    where flatboard      = map head $ group board
          initialpos     = length flatboard + 1
          calcrank score = foldr (\x acc -> if x <= score then acc-1 else acc) initialpos flatboard
