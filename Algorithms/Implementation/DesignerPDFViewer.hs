import Data.Char

main :: IO ()
main = do height <- getList
          word   <- getLine
          putStrLn $ show $ solve height word

getList :: Read a => IO [a]
getList = map read . words <$> getLine

solve :: [Int] -> String -> Int
solve hs word = tallest * (length word) 
    where posofa  = ord 'a'
          posofls = [hs !! (ord l - posofa) | l <- word]
          tallest = maximum posofls
