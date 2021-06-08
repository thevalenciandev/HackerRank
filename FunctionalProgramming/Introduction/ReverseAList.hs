rev :: [Int] -> [Int]
rev = foldr (\x acc -> acc ++ [x]) []
