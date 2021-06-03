import Data.Char
import Data.List

main = interact $ show . solve . head . words

solve :: String -> Int
solve xs = length (camelchunks xs)
          
camelchunks xs = fst firstsplit : humps
    where firstsplit = span isLower xs -- this will get separate the first word from the rest
          humps      = groupBy (\x y -> isUpper x && isLower y) $ snd firstsplit
