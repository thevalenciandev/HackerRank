main = interact $ show . solve . head . words

solve :: String -> Int
solve xs = foldr (\(x,y) acc -> if x/=y then acc+1 else acc) 0 (zip xs (cycle "SOS")) 
