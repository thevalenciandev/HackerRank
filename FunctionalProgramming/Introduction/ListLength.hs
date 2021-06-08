len :: [a] -> Int
len = foldr (\_ acc -> acc+1) 0
