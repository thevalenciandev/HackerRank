main :: IO ()
main = interact $ unlines . map show . solve . map read . words

solve :: [Int] -> [Int]
solve (s:t:a:b:m:n:rest) = [as,os]
    where as = countIn (take m rest) a (s,t)
          os = countIn (drop m rest) b (s,t)

countIn :: [Int] -> Int -> (Int, Int) -> Int
countIn fruitDis treeLoc (s,t) =
    length $ filter withinHouse $ map (+treeLoc) fruitDis 
    where withinHouse = \x -> x>=s && x<=t
