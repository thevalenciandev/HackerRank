main = do n <- read . head . words <$> getLine
          sequence . replicate n . putStrLn $ "Hello World"
