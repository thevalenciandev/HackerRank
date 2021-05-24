main :: IO ()
main = do [q]   <- getList
          lines <- sequence (replicate q getList)
          putStrLn $ unlines $ map solve lines
          
getList :: Read a => IO [a]
getList = map read . words <$> getLine

solve :: [Int] -> String
solve [x,y,z] | catA < catB = "Cat A"
              | catB < catA = "Cat B"
              | otherwise   = "Mouse C"
              where catA = abs (z - x)
                    catB = abs (z - y)
