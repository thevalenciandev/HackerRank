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
          calcrank score = (length $ takeWhile (>score) flatboard) + 1
