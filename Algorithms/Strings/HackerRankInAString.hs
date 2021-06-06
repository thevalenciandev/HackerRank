import Control.Monad.State

main = do n <- readLn
          inputs <- sequence (replicate n getLine)
          let solutions = map (\xs -> solve xs "hackerrank") inputs
          mapM_ putStrLn solutions

solve :: String -> String -> String
solve _ []              = "YES"
solve [] _              = "NO"
solve (x:xs) all@(y:ys) = if x == y then 
                              solve xs ys
                          else
                              solve xs all
