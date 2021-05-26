main :: IO ()
main = interact $ show . solve . map read . words

solve :: [Int] -> Int
solve (_:k:height) = if potiondoses < 0 then 0 else potiondoses 
    where potiondoses = maximum height - k
