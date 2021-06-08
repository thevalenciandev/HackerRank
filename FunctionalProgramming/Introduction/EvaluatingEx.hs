main = do n  <- readLn
          xs <- sequence (replicate n (read <$> getLine))
          let results = map solve xs
          mapM_ (putStrLn . show) results

solve :: Double -> Double
solve x = let series = [x**i / (product [1..i]) | i<-[0..]]
          in  sum $ take 10 series 
