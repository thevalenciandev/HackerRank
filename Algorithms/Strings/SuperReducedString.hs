import Data.List

main = interact $ solve . head . words

solve :: String -> String
solve xs | before /= after = solve (concat after)
         | otherwise = if xs == [] then "Empty String" else xs
    where removepairs g = if odd (length g) then [head g] else []
          before = group xs
          after  = map removepairs before
